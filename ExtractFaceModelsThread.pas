unit ExtractFaceModelsThread;
{$INCLUDE ARCFACE.INC}

interface

uses
  System.Classes, System.SysUtils, IPC.Events, DgsSynLocker, amcomDef, ammemDef,
  ArcFaceSDK, {$IFDEF ARC_RZ_SDK} arcsoft_fsdk_fic, {$ENDIF}
  ArcFaceSDKIEVersion,
  arcsoft_fsdk_face_detection, arcsoft_fsdk_face_recognition,
  arcsoft_fsdk_face_tracking, asvloffscreenDef, hyiedefs, iexBitmaps, hyieutils,
  System.Generics.Collections;

type

  TOnGetFaceModels = procedure(ACamreaBitmap: TIEBitmap;
    ACamreaFaceRegions: TList<AFR_FSDK_FACEINPUT>;
    AEdzFaceModels, AHistoryFaceModels: TFaceModels) of object;
  TOnGetSimilarity = procedure(ABitmap: TIEBitmap;
    AFaceRegion: AFR_FSDK_FACEINPUT; AFaceModel: AFR_FSDK_FACEMODEL;
    ASimilarity: Single; ACameraFaceCount, AStandardFaceCount: integer;
    ARyID, AZjhm: String) of object;

  TExtractFaceModelsThread = class(TThread)
  private
    { Private declarations }
    FSafe: TDgsSynLocker;
    FEvent: Event;
    FArcFaceSDK: TArcFaceSDKIEVersion;
    FCamreaBitmap: TIEBitmap;
    FUseIDCard: Boolean;
    FCamreaFaceRegions: TList<AFR_FSDK_FACEINPUT>;
    FStandardFaceModels: TEdzFaceModels;
    FHistoryFaceModels: TEdzFaceModels;
    FEnabled: Boolean;
    FOnGetFaceModels: TOnGetFaceModels;
    FOnGetSimilarity: TOnGetSimilarity;
    FSendZeroData: Boolean;
    procedure DoGetFaceModels;
    procedure Lock;
    function NewGuid: String;
    procedure Unlock;
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended, AUseIDCard: Boolean);
    destructor Destroy; override;
    procedure EndThread;
    function GetData: Boolean;
    procedure MatchFace;
    procedure Signal;
    property Enabled: Boolean read FEnabled write FEnabled;
    property SendZeroData: Boolean read FSendZeroData write FSendZeroData;
    property OnGetSimilarity: TOnGetSimilarity read FOnGetSimilarity write
      FOnGetSimilarity;
    property OnGetFaceModels: TOnGetFaceModels read FOnGetFaceModels write
      FOnGetFaceModels;
  end;

implementation


constructor TExtractFaceModelsThread.Create(CreateSuspended, AUseIDCard:
  Boolean);
begin
  inherited Create(CreateSuspended);
  FSendZeroData := False;
  FUseIDCard := AUseIDCard;
  FSafe.Init;
  FEvent := Event.Create(NewGuid, False, False, False);
  FCamreaBitmap := TIEBitmap.Create;
  FCamreaFaceRegions := TList<AFR_FSDK_FACEINPUT>.Create;
  FStandardFaceModels := TEdzFaceModels.Create;
  FHistoryFaceModels := TEdzFaceModels.Create;
  FArcFaceSDK := TArcFaceSDKIEVersion.Create;
  FArcFaceSDK.InitialFaceRecognitionEngine(False);
{$IFDEF ARC_RZ_SDK}
  FArcFaceSDK.InitialFaceRzFicEngine(False);
{$ENDIF}
  FEnabled := True;
end;

destructor TExtractFaceModelsThread.Destroy;
begin
  FCamreaBitmap.Free;
  FCamreaFaceRegions.Free;
  FStandardFaceModels.Free;
  FHistoryFaceModels.Free;
  FArcFaceSDK.Free;
  FSafe.Done;
  inherited;
end;

procedure TExtractFaceModelsThread.DoGetFaceModels;
var
  lSimilarity: Single;
begin
  if Assigned(FOnGetFaceModels) then
  begin
    if GetData then
      MatchFace;
  end;
end;

procedure TExtractFaceModelsThread.EndThread;
begin
  Terminate;
  FEvent.Signal;
end;

{ TExtractFaceModelsThread }

procedure TExtractFaceModelsThread.Execute;
begin
  while not Terminated do
  begin
    FEvent.WaitForSignal;
    if Terminated then
      Break;
    if FEnabled then
      DoGetFaceModels;
  end;
end;

function TExtractFaceModelsThread.GetData: Boolean;
begin
  Result := False;
  Lock;
  try
    try
      FOnGetFaceModels(FCamreaBitmap, FCamreaFaceRegions,
        FStandardFaceModels, FHistoryFaceModels);
      Result := True;
    except
      Result := False;
    end;
  finally
    Unlock;
  end;
end;

procedure TExtractFaceModelsThread.Lock;
begin
  FSafe.Lock;
end;

procedure TExtractFaceModelsThread.MatchFace;
var
  lFaceModel, lFaceModelTmp: AFR_FSDK_FACEMODEL;
{$IFDEF ARC_RZ_SDK}
  lRzFaceModel: AFIC_FSDK_FACERES;
{$ENDIF}
  siSimilarity, siTmp: Single;
  i, j, iFaceRegion, iCompareResult: integer;
  iCamreaFaceCount, iStandardFaceCount, iHistoryFaceCount: integer;
  sRyID, sZjhm: string;
  lFaceRegion: AFR_FSDK_FACEINPUT;
  lBitmap: TIEBitmap;
begin
  siSimilarity := -1;
  iFaceRegion := -1;
  iCompareResult := 0;
  lFaceModel.pbFeature := nil;
  lFaceModelTmp.pbFeature := nil;
  try
    try
      iCamreaFaceCount := FCamreaFaceRegions.Count;
      iStandardFaceCount := FStandardFaceModels.Count;
      iHistoryFaceCount := FHistoryFaceModels.Count;
      sRyID := FStandardFaceModels.RyID;
      sZjhm := FStandardFaceModels.Params;

      with lFaceModel do
      begin
        pbFeature := nil; // The extracted features
        lFeatureSize := 0;
      end;

      if (iCamreaFaceCount > 0) then
      begin
{$IFDEF ARC_RZ_SDK}
        // 使用二代证照片SDK
        if FUseIDCard and (iStandardFaceCount > 0) then
        begin
          if FStandardFaceModels.Changed then
          begin
            lBitmap := TIEBitmap.Create;
            try
              lBitmap.Assign(FStandardFaceModels.Bitmap);
              FArcFaceSDK.RzFicIdCardDataFeatureExtractionFromIEBitmap
                (lBitmap, nil, 0, 0, rfNone, 0);
              FStandardFaceModels.ResetState;
            finally
              lBitmap.Free;
            end;
          end;

          FArcFaceSDK.RzFicFaceDataFeatureExtractionFromIEBitmap(FCamreaBitmap,
            True, lRzFaceModel, nil, 0, 0, rfNone, 0, True);

          FArcFaceSDK.RzFicFaceIdCardCompare(siSimilarity, iCompareResult);

          iFaceRegion := 0;
          lFaceRegion.lOrient := 0;
          lFaceRegion.rcFace := lRzFaceModel.rcFace;

          if Terminated then
            Exit;
          if Assigned(FOnGetSimilarity) then
            Synchronize(
              procedure()
              begin
                FOnGetSimilarity(FCamreaBitmap,
                  lFaceRegion,
                  lFaceModel, siSimilarity, iCamreaFaceCount,
                  iStandardFaceCount, sRyID, sZjhm);
              end);
        end
        else
        begin
{$ENDIF}
          if (iStandardFaceCount + iHistoryFaceCount > 0) then
            for i := 0 to iCamreaFaceCount - 1 do
            begin
              if Terminated then
                Exit;

              // 提取人脸特征
              if FArcFaceSDK.ExtractFaceFeatureFromIEBitmap(FCamreaBitmap,
                FCamreaFaceRegions.Items[i], lFaceModelTmp, nil, 0, 0, rfNone,
                True)
              then
              begin
                try
                  // 处理标准人脸特征
                  for j := 0 to iStandardFaceCount - 1 do
                  begin
                    if Terminated then
                      Exit;
                    siTmp := FArcFaceSDK.MatchFace
                      (FStandardFaceModels.FaceModel[j],
                      lFaceModelTmp);
                    if siTmp > siSimilarity then
                    begin
                      siSimilarity := siTmp;
                      iFaceRegion := i;
                      lFaceModel := lFaceModelTmp;
                    end;
                  end;
                  // 处理历史人脸特征
                  for j := 0 to iHistoryFaceCount - 1 do
                  begin
                    if Terminated then
                      Exit;
                    siTmp := FArcFaceSDK.MatchFace
                      (FHistoryFaceModels.FaceModel[j],
                      lFaceModelTmp);
                    if siTmp > siSimilarity then
                    begin
                      siSimilarity := siTmp;
                      iFaceRegion := i;
                      lFaceModel := lFaceModelTmp;
                    end;
                  end;
                finally
                  if (lFaceModelTmp.pbFeature <> lFaceModel.pbFeature) and
                    (lFaceModelTmp.pbFeature <> nil) then
                  begin
                    FreeMem(lFaceModelTmp.pbFeature);
                    lFaceModelTmp.pbFeature := nil;
                  end;
                end;
              end;
            end;

          if Terminated then
            Exit;
          if (iFaceRegion < 0) then
          begin
            if iHistoryFaceCount + iStandardFaceCount > 0 then
            begin
              FArcFaceSDK.ExtractFaceFeatureFromIEBitmap(FCamreaBitmap,
                FCamreaFaceRegions.Items[0], lFaceModel, nil, 0, 0,
                rfNone, True);
            end;
            iFaceRegion := 0;
          end;

          if Terminated then
            Exit;
          if iFaceRegion >= 0 then
          begin
            lFaceRegion := FCamreaFaceRegions.Items[iFaceRegion];
            if Assigned(FOnGetSimilarity) then
              Synchronize(
                procedure()
                begin
                  FOnGetSimilarity(FCamreaBitmap,
                    lFaceRegion,
                    lFaceModel, siSimilarity, iCamreaFaceCount,
                    iStandardFaceCount, sRyID, sZjhm);
                end);
          end;
{$IFDEF ARC_RZ_SDK}
        end;
{$ENDIF}
      end
      else
      begin
        if Terminated then
          Exit;

        // 强制传送空数据
        if SendZeroData then
        begin
          if Assigned(FOnGetSimilarity) then
            Synchronize(
              procedure()
              begin
                FOnGetSimilarity(FCamreaBitmap,
                  lFaceRegion,
                  lFaceModel, 0, 0,
                  iStandardFaceCount, sRyID, sZjhm);
              end);
        end;
      end;

    finally
      if lFaceModel.pbFeature <> lFaceModelTmp.pbFeature then
      begin
        if lFaceModel.pbFeature <> nil then
          FreeMem(lFaceModel.pbFeature);

        if lFaceModelTmp.pbFeature <> nil then
          FreeMem(lFaceModelTmp.pbFeature);
      end
      else
      begin
        if lFaceModel.pbFeature <> nil then
          FreeMem(lFaceModel.pbFeature);
      end;

      lFaceModel.pbFeature := nil;
      lFaceModelTmp.pbFeature := nil;
    end;
  except
    siSimilarity := -1;
    iFaceRegion := -1;
  end;
end;

function TExtractFaceModelsThread.NewGuid: String;
var
  LTep: TGUID;
  sGUID: string;
begin
  CreateGUID(LTep);
  sGUID := GUIDToString(LTep);
  sGUID := StringReplace(sGUID, '-', '', [rfReplaceAll]);
  sGUID := Copy(sGUID, 2, Length(sGUID) - 2);
  Result := sGUID;
end;

procedure TExtractFaceModelsThread.Signal;
begin
  FEvent.Signal;
end;

procedure TExtractFaceModelsThread.Unlock;
begin
  FSafe.Unlock;
end;

end.
