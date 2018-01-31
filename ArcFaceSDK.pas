(*******************************************************
 * 虹软人脸识别SDK封装 TBitmap 版
 * 版权所有 (C) 2017 NJTZ  eMail:yhdgs@qq.com
 *******************************************************)

unit ArcFaceSDK;

interface

uses Windows, Messages, SysUtils, System.Classes, math,
  amcomdef, ammemDef,
  arcsoft_fsdk_face_detection,
  arcsoft_fsdk_face_recognition,
  arcsoft_fsdk_face_tracking, asvloffscreendef, merrorDef,
  arcsoft_fsdk_age_estimation, arcsoft_fsdk_gender_estimation,
  Vcl.Graphics, Vcl.Imaging.jpeg, System.Generics.Collections;

type

  TOnLogEvent = procedure(Const Msg: String) of object;

  TImgDataInfo = record
    pImgData: PByte;
    Width: Integer;
    Height: Integer;
    LineBytes: Integer;
    BitCount: Integer;
  end;

  TFaceBaseInfo = record
    FaceRect: MRECT;
    FaceOrient: Integer;
    Age: Integer;
    Gender: Integer;
  private
  public
    procedure Init;
  end;

  TFaceFullInfo = record
    FaceRect: MRECT;
    FaceOrient: Integer;
    Age: Integer;
    Gender: Integer;
    Model: AFR_FSDK_FACEMODEL;
  private
  public
    procedure Init;
  end;

  TFaceModels = class(TObject)
  private
    FModels: TList<AFR_FSDK_FACEMODEL>;
    function GetCount: Integer;
    function GetFaceModel(Index: Integer): AFR_FSDK_FACEMODEL;
    function GetItems(Index: Integer): AFR_FSDK_FACEMODEL;
  public
    constructor Create;
    destructor Destroy; override;
    function AddModel(AModel: AFR_FSDK_FACEMODEL): Integer;
    procedure Assign(ASource: TFaceModels); virtual;
    procedure AddModels(ASource: TFaceModels);
    procedure Clear; virtual;
    procedure Delete(Index: Integer);
    property Count: Integer read GetCount;
    property FaceModel[Index: Integer]: AFR_FSDK_FACEMODEL read GetFaceModel;
    property Items[Index: Integer]: AFR_FSDK_FACEMODEL read GetItems;
  end;

  TArcFaceSDK = class(TObject)
  private
    FAppID: string;
    FEstimationBufferSize: Int64;
    FFaceAgeKey: String;
    FFaceRecognitionKey: string;
    FFaceTrackingKey: string;
    FFaceDetectionKey: string;
    FFaceGenderKey: String;
    FMaxFace: Integer;
    FScale: Integer;
    FWorkKBufferSize: Int64;
    FOnLog: TOnLogEvent;
    FOrientPriority: TAFD_FSDK_OrientPriority;
    FpFaceRecognitionBuf: PByte;
    FpFaceTrackingBuf: PByte;
    FpFaceDetectionBuf: PByte;
    FpFaceAgeBuf: PByte;
    FpFaceGenderBuf: PByte;
    procedure SetMaxFace(const Value: Integer);
    procedure SetScale(const Value: Integer);
  protected
    FFaceRecognitionEngine: MHandle;
    FFaceTrackingEngine: MHandle;
    FFaceDetectionEngine: MHandle;
    FFaceAgeEngine: MHandle;
    FFaceGenderEngine: MHandle;
    procedure DoLog(const Msg: String);
  public
    constructor Create;
    destructor Destroy; override;
    function DetectAndRecognitionFacesFromBmp(ABitmap: TBitmap; var AFaceRegions:
      TList<AFR_FSDK_FACEINPUT>; var AFaceModels: TFaceModels): Boolean;
    function DetectFacesAndAgeGenderFromBitmap(ABitmap: TBitmap; var AFaceInfos:
      TList<TFaceBaseInfo>): Boolean;
    function DetectFacesFromBmp(ABitmap: TBitmap; var AFaceRegions:
      TList<AFR_FSDK_FACEINPUT>): Boolean;
    function TrackFacesFromBmp(ABitmap: TBitmap; var AFaceRegions:
      TList<AFR_FSDK_FACEINPUT>): Boolean;
    function DetectFacesFromBmpFile(AFile: string; var AFaceRegions:
      TList<AFR_FSDK_FACEINPUT>): Boolean;
    function DRAGfromJPGFile(AFileName: string; var AFaceInfos:
      TList<TFaceBaseInfo>; var AFaceModels: TFaceModels): Boolean; overload;
    function DRAGfromBmp(ABitmap: TBitmap; var AFaceInfos: TList<TFaceBaseInfo>;
      var AFaceModels: TFaceModels): Boolean; overload;
    function DRAGfromBmpFile(AFileName: string; var AFaceInfos:
      TList<TFaceBaseInfo>; var AFaceModels: TFaceModels): Boolean; overload;
    class procedure DrawFaceRectAgeGender(ACanvas: TCanvas; AFaceIdx: Integer;
      AFaceInfo: TFaceBaseInfo; AColor: TColor = clBlue; AWidth: Integer = 2;
      ADrawIndex: Boolean = true; ATextSize: Integer = 0);
    class procedure DrawFaceRect(ACanvas: TCanvas; AFaceIdx: Integer; AFaceInfo:
      AFR_FSDK_FACEINPUT; AColor: TColor = clBlue; AWidth: Integer = 2;
      ADrawIndex: Boolean = true; ATextSize: Integer = 0);
    function TrackFacesFromBmpFile(AFile: string; var AFaceRegions:
      TList<AFR_FSDK_FACEINPUT>): Boolean;
    class procedure ExtractFaceBoxs(AFaces: AFD_FSDK_FACERES; AFaceRegions:
      TList<AFR_FSDK_FACEINPUT>); overload;
    class procedure ExtractFaceBoxs(AFaces: AFT_FSDK_FACERES; var AFaceRegions:
      TList<AFR_FSDK_FACEINPUT>); overload;
    class procedure ExtractFaceAges(AFaceAgeResults: ASAE_FSDK_AGERESULT; var
      AFaceAges: TArray<Integer>); overload;
    class procedure ExtractFaceGenders(AFaceGenderResults
      : ASGE_FSDK_GENDERRESULT;
      var AFaceGenders: TArray<Integer>); overload;
    function ExtractFaceFeature(AFaceInput: ASVLOFFSCREEN; AFaceRegion:
      AFR_FSDK_FACEINPUT; var AFaceModel: AFR_FSDK_FACEMODEL): Boolean;
    function ExtractFaceFeatures(AFaceInput: ASVLOFFSCREEN; AFaceRegions:
      TList<AFR_FSDK_FACEINPUT>; var AFaceModels: TFaceModels): Boolean;
    function ExtractFaceFeatureFromBmp(ABitmap: TBitmap; AFaceRegion:
      AFR_FSDK_FACEINPUT; var AFaceModel: AFR_FSDK_FACEMODEL): Boolean;
    function ExtractFaceFeaturesFromBmp(ABitmap: TBitmap; AFaceRegions:
      TList<AFR_FSDK_FACEINPUT>; var AFaceModels: TFaceModels): Boolean;
    function ExtractFaceFeatureFromBmpFile(AFile: string; AFaceRegion:
      AFR_FSDK_FACEINPUT; var AFaceModel: AFR_FSDK_FACEMODEL): Boolean;
    function ExtractFaceFeaturesFromBmpFile(AFile: string; AFaceRegions:
      TList<AFR_FSDK_FACEINPUT>; var AFaceModels: TFaceModels): Boolean;
    function InitialFaceDetectionEngine(Deinitial: Boolean): Integer;
    function InitialFaceTrackingEngine(Deinitial: Boolean): Integer;
    function InitialFaceRecognitionEngine(Deinitial: Boolean): Integer;
    function InitialFaceAgeEngine(Deinitial: Boolean): Integer;
    function InitialFaceGenderEngine(Deinitial: Boolean): Integer;
    function MatchFace(AFaceModel1, AFaceModel2: AFR_FSDK_FACEMODEL): Single;
    function MatchFaceWithBitmaps(ABitmap1, ABitmap2: TBitmap): Single;
    function UnInitialFaceDetectionEngine: Integer;
    function UnInitialFaceTrackingEngine: Integer;
    function UnInitialFaceRecognitionEngine: Integer;
    class function ReadBmp(ABitmap: TBitmap; var AImgData: PByte; var AWidth,
      AHeight, ALineBytes: Integer): Boolean;
    class function ReadBmpFile(AFileName: string; var AImgData: PByte;
      var AWidth,
      AHeight, ALineBytes: Integer): Boolean; overload;
    class function ReadBmpFile(AFileName: string; ABitmap: TBitmap): Boolean;
      overload;
    class function ReadJpegFile(AFileName: string; var AImgData: PByte;
      var AWidth,
      AHeight, ABitCount: Integer): Boolean; overload;
    class function ReadBmpStream(AStream: TMemoryStream; var AImgData: PByte; var
      AWidth, AHeight, ALineBytes: Integer): Boolean;
    class function ReadJpegFile(AFileName: string; ABitmap: TBitmap): Boolean;
      overload;
    function TrackFacesAndAgeGenderFromBmp(ABitmap: TBitmap; var AFaceInfos:
      TList<TFaceBaseInfo>): Boolean;
    function UnInitialFaceAgeEngine: Integer;
    function UnInitialFaceGenderEngine: Integer;
    property AppID: String read FAppID write FAppID;
    property FaceDetectionKey: string read FFaceDetectionKey write
      FFaceDetectionKey;
    property FaceRecognitionKey: string read FFaceRecognitionKey write
      FFaceRecognitionKey;
    property FaceTrackingKey: string read FFaceTrackingKey
      write FFaceTrackingKey;
    property MaxFace: Integer read FMaxFace write SetMaxFace;
    property OnLog: TOnLogEvent read FOnLog write FOnLog;
    property EstimationBufferSize: Int64 read FEstimationBufferSize write
      FEstimationBufferSize;
    property FaceAgeKey: String read FFaceAgeKey write FFaceAgeKey;
    property FaceGenderKey: String read FFaceGenderKey write FFaceGenderKey;
    property OrientPriority: TAFD_FSDK_OrientPriority read FOrientPriority write
      FOrientPriority;
    property Scale: Integer read FScale write SetScale;
    property WorkKBufferSize: Int64 read FWorkKBufferSize
      write FWorkKBufferSize;
  end;

  TuJpegImage = class(TJPEGImage)
  public
    function BitmapData: TBitmap;
  end;

  TEdzFaceModels = class(TFaceModels)
  private
    FRyID: String;
    FParams: String;
  public
    constructor Create;
    procedure Assign(ASource: TFaceModels); override;
    procedure Clear; override;
    property RyID: String read FRyID write FRyID;
    property Params: String read FParams write FParams;
  end;

implementation


constructor TArcFaceSDK.Create;
begin
  inherited;

  FAppID := Your Key;
  //人脸检测(FD) Key
  FFaceDetectionKey := 'Your Key';
  //人脸识别(FR) Key
  FFaceRecognitionKey := 'Your Key';
  //人脸追踪(FT) Key
  FFaceTrackingKey := 'Your Key';
  //年龄识别(Age)Key
  FFaceAgeKey := 'Your Key';
  //性别识别(Gender)Key
  FFaceGenderKey := 'Your Key';

  FWorkKBufferSize := 40 * 1024 * 1024;
  FEstimationBufferSize := 30 * 1024 * 1024;
  FScale := 16;
  FMaxFace := 10;
  FOrientPriority := TAFD_FSDK_OrientPriority.AFD_FSDK_OPF_0_HIGHER_EXT;

  FFaceDetectionEngine := nil;
  FFaceRecognitionEngine := nil;
  FFaceTrackingEngine := nil;
  FFaceAgeEngine := nil;
  FFaceGenderEngine := nil;

  FpFaceDetectionBuf := nil;
  FpFaceRecognitionBuf := nil;
  FpFaceTrackingBuf := nil;
  FpFaceAgeBuf := nil;
  FpFaceGenderBuf := nil;

end;

destructor TArcFaceSDK.Destroy;
begin
  UnInitialFaceDetectionEngine;
  UnInitialFaceTrackingEngine;
  UnInitialFaceRecognitionEngine;
  UnInitialFaceAgeEngine;
  UnInitialFaceGenderEngine;
  inherited;
end;

{-------------------------------------------------------------------------------
 过程名:    TArcFaceSDK.DetectAndRecognitionFacesFromBmp
 功能:      检测人脸位置并提取特征（支持多人脸）
 作者:      Bird
 日期:      2017.11.19
 参数:      ABitmap: TBitmap; //源位图
 var AFaceRegions: TList<AFR_FSDK_FACEINPUT>; //输出人脸位置信息列表
 var AFaceModels: TFaceModels //输出人脸特征信息
 返回值:    Boolean
 -------------------------------------------------------------------------------}
function TArcFaceSDK.DetectAndRecognitionFacesFromBmp(ABitmap: TBitmap; var
  AFaceRegions: TList<AFR_FSDK_FACEINPUT>; var AFaceModels: TFaceModels):
  Boolean;
var
  pImgData: PByte;
  iWidth, iHeight, iLineBytes: Integer;
  offInput: ASVLOFFSCREEN;
  pFaceRes: LPAFD_FSDK_FACERES;
  nRet: MRESULT;
{$IFDEF DEBUG}
  T: Cardinal;
{$ENDIF}
begin
  Result := False;

  if FFaceDetectionEngine = nil then
    Exit;

  if AFaceRegions = nil then
    AFaceRegions := TList<AFR_FSDK_FACEINPUT>.Create;

  pImgData := nil;
{$IFDEF DEBUG}
  T := GetTickCount;
{$ENDIF}
  if not ReadBmp(ABitmap, pImgData, iWidth, iHeight, iLineBytes) then
    Exit;
{$IFDEF DEBUG}
  T := GetTickCount - T;
  DoLog('载入数据耗时：' + IntToStr(T));
{$ENDIF}
  offInput.u32PixelArrayFormat := ASVL_PAF_RGB24_B8G8R8;
  FillChar(offInput.pi32Pitch, SizeOf(offInput.pi32Pitch), 0);
  FillChar(offInput.ppu8Plane, SizeOf(offInput.ppu8Plane), 0);

  offInput.i32Width := iWidth;
  offInput.i32Height := iHeight;

  offInput.ppu8Plane[0] := IntPtr(pImgData);
  offInput.pi32Pitch[0] := iLineBytes;
  //人脸检测
{$IFDEF DEBUG}
  T := GetTickCount;
{$ENDIF}
  nRet := AFD_FSDK_StillImageFaceDetection(FFaceDetectionEngine, @offInput,
    pFaceRes);
{$IFDEF DEBUG}
  T := GetTickCount - T;
  DoLog('检测人脸耗时：' + IntToStr(T));
{$ENDIF}
  if nRet = MOK then
  begin
    ExtractFaceBoxs(pFaceRes^, AFaceRegions);

    if AFaceModels = nil then
      AFaceModels := TFaceModels.Create;
{$IFDEF DEBUG}
    T := GetTickCount;
{$ENDIF}
    Result := ExtractFaceFeatures(offInput, AFaceRegions, AFaceModels);
{$IFDEF DEBUG}
    T := GetTickCount - T;
    DoLog('提取特征耗时：' + IntToStr(T));
{$ENDIF}
  end;

  if pImgData <> nil then
    FreeMem(pImgData);

end;

{-------------------------------------------------------------------------------
 过程名:    TArcFaceSDK.TrackFacesAndAgeGenderFromIEBitmap1
 功能:      从Bitmap中获取人脸位置、性别和年龄信息列表
 作者:      DelphiDX10
 日期:      2018.01.31
 参数:      ABitmap: TBitmap; var AFaceInfos: TList<TFaceBaseInfo>
 返回值:    Boolean
 -------------------------------------------------------------------------------}
function TArcFaceSDK.DetectFacesAndAgeGenderFromBitmap(ABitmap: TBitmap; var
  AFaceInfos: TList<TFaceBaseInfo>): Boolean;
var
  pImgData: PByte;
  offInput: ASVLOFFSCREEN;
  pFaceRes: LPAFD_FSDK_FACERES;
  lFaceRes_Age: ASAE_FSDK_AGEFACEINPUT;
  lFaceRes_Gender: ASGE_FSDK_GENDERFACEINPUT;
  lFaceRegions: TList<AFR_FSDK_FACEINPUT>;
  lAgeRes: ASAE_FSDK_AGERESULT;
  lGenderRes: ASGE_FSDK_GENDERRESULT;
  lAges: TArray<Integer>;
  lGenders: TArray<Integer>;
  lFaceInfo: TFaceBaseInfo;
  i, iFaces: Integer;
  iWidth, iHeight, iLineBytes: Integer;
  ArrFaceOrient: array of AFD_FSDK_OrientCode;
  ArrFaceRect: array of MRECT;
begin
  Result := False;

  if AFaceInfos = nil then
    AFaceInfos := TList<TFaceBaseInfo>.Create;

  if FFaceDetectionEngine = nil then
    Exit;

  pImgData := nil;

  if not ReadBmp(ABitmap, pImgData, iWidth, iHeight, iLineBytes)
  then
    Exit;

  offInput.u32PixelArrayFormat := ASVL_PAF_RGB24_B8G8R8;
  FillChar(offInput.pi32Pitch, SizeOf(offInput.pi32Pitch), 0);
  FillChar(offInput.ppu8Plane, SizeOf(offInput.ppu8Plane), 0);

  offInput.i32Width := iWidth;
  offInput.i32Height := iHeight;

  offInput.ppu8Plane[0] := IntPtr(pImgData);
  offInput.pi32Pitch[0] := iLineBytes;

  lFaceRegions := TList<AFR_FSDK_FACEINPUT>.Create;
  try
    //人脸检测
    if AFD_FSDK_StillImageFaceDetection(FFaceDetectionEngine, @offInput,
      pFaceRes) = MOK then
    begin
      //分解人脸位置信息
      ExtractFaceBoxs(pFaceRes^, lFaceRegions);
      if lFaceRegions.Count > 0 then
      begin
        iFaces := lFaceRegions.Count;
        SetLength(ArrFaceOrient, iFaces);
        SetLength(ArrFaceRect, iFaces);
        for i := 0 to iFaces - 1 do
        begin
          ArrFaceOrient[i] := lFaceRegions.Items[i].lOrient;
          ArrFaceRect[i] := lFaceRegions.Items[i].rcFace;
        end;

        //检测年龄
        if (FFaceAgeEngine <> nil) then
        begin
          with lFaceRes_Age do
          begin
            pFaceRectArray := @ArrFaceRect[0];
            pFaceOrientArray := @ArrFaceOrient[0];
            lFaceNumber := iFaces;
          end;

          if ASAE_FSDK_AgeEstimation_StaticImage(
            FFaceAgeEngine, //[in] age estimation engine
            @offInput, //[in] the original image information
            //[in] the face rectangles information
            @lFaceRes_Age,
            //[out] the results of age estimation
            lAgeRes
            ) = MOK then
            //分解人脸年龄
            ExtractFaceAges(lAgeRes, lAges);

        end;

        //检测性别
        if (FFaceGenderEngine <> nil) then
        begin
          with lFaceRes_Gender do
          begin
            pFaceRectArray := @ArrFaceRect[0];
            pFaceOrientArray := @ArrFaceOrient[0];
            lFaceNumber := iFaces;
          end;

          if ASGE_FSDK_GenderEstimation_StaticImage(
            FFaceGenderEngine, //[in] Gender estimation engine
            @offInput, //[in] the original imGender information
            //[in] the face rectangles information
            @lFaceRes_Gender,
            //[out] the results of Gender estimation
            lGenderRes
            ) = MOK then
            //分解人脸性别
            ExtractFaceGenders(lGenderRes, lGenders);

        end;

        for i := 0 to iFaces - 1 do
        begin
          lFaceInfo.Init;
          lFaceInfo.FaceRect := ArrFaceRect[i];
          lFaceInfo.FaceOrient := ArrFaceOrient[i];
          if i < Length(lAges) then
            lFaceInfo.Age := lAges[i];
          if i < Length(lGenders) then
            lFaceInfo.Gender := lGenders[i];
          AFaceInfos.Add(lFaceInfo);
        end;
      end;

    end;
  finally
    FreeAndNil(lFaceRegions);
  end;

  if pImgData <> nil then
    FreeMem(pImgData)

end;

{-------------------------------------------------------------------------------
 过程名:    TArcFaceSDK.DetectFacesFromBmp
 功能:      从位图中获取人脸位置信息列表
 作者:      Bird
 日期:      2017.11.19
 参数:      ABitmap: TBitmap;//源位图
 var AFaceRegions: TList<AFR_FSDK_FACEINPUT> //输出人脸位置列表
 返回值:    Boolean
 -------------------------------------------------------------------------------}
function TArcFaceSDK.DetectFacesFromBmp(ABitmap: TBitmap; var AFaceRegions:
  TList<AFR_FSDK_FACEINPUT>): Boolean;
var
  pImgData: PByte;
  iWidth, iHeight, iLineBytes: Integer;
  offInput: ASVLOFFSCREEN;
  pFaceRes: LPAFD_FSDK_FACERES;
begin
  Result := False;

  if FFaceDetectionEngine = nil then
    Exit;

  pImgData := nil;

  //读取位图到内存中
  if not ReadBmp(ABitmap, pImgData, iWidth, iHeight, iLineBytes) then
    Exit;

  offInput.u32PixelArrayFormat := ASVL_PAF_RGB24_B8G8R8;
  FillChar(offInput.pi32Pitch, SizeOf(offInput.pi32Pitch), 0);
  FillChar(offInput.ppu8Plane, SizeOf(offInput.ppu8Plane), 0);

  offInput.i32Width := iWidth;
  offInput.i32Height := iHeight;

  offInput.ppu8Plane[0] := IntPtr(pImgData);
  offInput.pi32Pitch[0] := iLineBytes;
  //调用API检测人脸
  if AFD_FSDK_StillImageFaceDetection(FFaceDetectionEngine, @offInput, pFaceRes)
    = MOK then
  begin
    //提取人脸位置框信息到列表
    ExtractFaceBoxs(pFaceRes^, AFaceRegions);
  end;

  if pImgData <> nil then
    FreeMem(pImgData);

end;

{-------------------------------------------------------------------------------
 过程名:    TArcFaceSDK.TrackFacesFromBmp
 功能:      获取人脸位置信息列表
 作者:      Bird
 日期:      2017.09.24
 参数:      ABitmap: TBitmap; //源位图
 var AFaceRegions: TList<AFR_FSDK_FACEINPUT> //输出人脸位置信息列表
 返回值:    Boolean
 -------------------------------------------------------------------------------}
function TArcFaceSDK.TrackFacesFromBmp(ABitmap: TBitmap; var AFaceRegions:
  TList<AFR_FSDK_FACEINPUT>): Boolean;
var
  pImgData: PByte;
  iWidth, iHeight, iLineBytes: Integer;
  offInput: ASVLOFFSCREEN;
  pFaceRes: LPAFT_FSDK_FACERES;
begin
  Result := False;

  if FFaceTrackingEngine = nil then
    Exit;

  pImgData := nil;

  if not ReadBmp(ABitmap, pImgData, iWidth, iHeight, iLineBytes) then
    Exit;

  offInput.u32PixelArrayFormat := ASVL_PAF_RGB24_B8G8R8;
  FillChar(offInput.pi32Pitch, SizeOf(offInput.pi32Pitch), 0);
  FillChar(offInput.ppu8Plane, SizeOf(offInput.ppu8Plane), 0);

  offInput.i32Width := iWidth;
  offInput.i32Height := iHeight;

  offInput.ppu8Plane[0] := IntPtr(pImgData);
  offInput.pi32Pitch[0] := iLineBytes;
  //offInput.pi32Pitch[1] := offInput.i32Width div 2;
  //offInput.pi32Pitch[2] := offInput.i32Width div 2;
  //人脸检测
  if AFT_FSDK_FaceFeatureDetect(FFaceTrackingEngine, @offInput, pFaceRes)
    = MOK then
  begin
    ExtractFaceBoxs(pFaceRes^, AFaceRegions);
  end;

  if pImgData <> nil then
    FreeMem(pImgData);

end;

{-------------------------------------------------------------------------------
 过程名:    TArcFaceSDK.DetectFacesFromBmpFile
 功能:      从文件Bmp文件中获取人脸位置信息列表，请确保文件为正确的BMP格式
 作者:      Bird
 日期:      2017.09.24
 参数:      AFile: String; //源文件
 var AFaceRegions: TList<AFR_FSDK_FACEINPUT> //输入人脸位置信息列表
 返回值:    Boolean
 -------------------------------------------------------------------------------}
function TArcFaceSDK.DetectFacesFromBmpFile(AFile: string; var AFaceRegions:
  TList<AFR_FSDK_FACEINPUT>): Boolean;
var
  BMP: TBitmap;
begin
  Result := False;
  if FFaceDetectionEngine = nil then
    Exit;

  if not FileExists(AFile) then
    Exit;
  BMP := TBitmap.Create;
  try
    BMP.LoadFromFile(AFile);
    Result := DetectFacesFromBmp(BMP, AFaceRegions);
  finally
    BMP.Free;
  end;
end;

{-------------------------------------------------------------------------------
 过程名:    TArcFaceSDK.TrackFacesFromBmpFile
 功能:      从文件中获取人脸位置信息列表（追踪模式）
 作者:      Bird
 日期:      2017.09.24
 参数:      AFile: String; //源文件
 var AFaceRegions: TList<AFR_FSDK_FACEINPUT> //输出人脸位置信息
 返回值:    Boolean
 -------------------------------------------------------------------------------}
function TArcFaceSDK.TrackFacesFromBmpFile(AFile: string; var AFaceRegions:
  TList<AFR_FSDK_FACEINPUT>): Boolean;
var
  BMP: TBitmap;
begin
  Result := False;
  if FFaceTrackingEngine = nil then
    Exit;

  if not FileExists(AFile) then
    Exit;
  BMP := TBitmap.Create;
  try
    //载入文件
    BMP.LoadFromFile(AFile);
    //检测人脸
    Result := TrackFacesFromBmp(BMP, AFaceRegions);
  finally
    BMP.Free;
  end;
end;

procedure TArcFaceSDK.DoLog(const Msg: String);
begin
  if Assigned(FOnLog) then
    FOnLog(Msg);
end;

{-------------------------------------------------------------------------------
  过程名:    TArcFaceSDK.DRAGfromJPGFile
  功能:      从JPG文件中获取人脸位置、年龄、性别和特征信息列表
  作者:      Bird
  日期:      2018.01.31
  参数:      AFileName: string; var AFaceInfos: TList<TFaceBaseInfo>; var AFaceModels: TFaceModels
  返回值:    Boolean
-------------------------------------------------------------------------------}
function TArcFaceSDK.DRAGfromJPGFile(AFileName: string; var AFaceInfos:
  TList<TFaceBaseInfo>; var AFaceModels: TFaceModels): Boolean;
var
  lBitmap: TBitmap;
begin
  Result := False;
  if not FileExists(AFileName) then
    Exit;

  lBitmap := TBitmap.Create;
  try
    if ReadJpegFile(AFileName, lBitmap) then
      Result := DRAGfromBmp(lBitmap, AFaceInfos, AFaceModels);
  finally
    lBitmap.Free;
  end;
end;

{-------------------------------------------------------------------------------
  过程名:    TArcFaceSDK.DRAGfromBmp
  功能:      从Bitmap中获取人脸位置、年龄、性别和特征信息列表
  作者:      Bird
  日期:      2018.01.31
  参数:      ABitmap: TBitmap; var AFaceInfos: TList<TFaceBaseInfo>; var AFaceModels: TFaceModels
  返回值:    Boolean
-------------------------------------------------------------------------------}
function TArcFaceSDK.DRAGfromBmp(ABitmap: TBitmap; var AFaceInfos:
  TList<TFaceBaseInfo>; var AFaceModels: TFaceModels): Boolean;
var
  pImgData: PByte;
  offInput: ASVLOFFSCREEN;
  pFaceRes: LPAFD_FSDK_FACERES;
  lFaceRes_Age: ASAE_FSDK_AGEFACEINPUT;
  lFaceRes_Gender: ASGE_FSDK_GENDERFACEINPUT;
  nRet: MRESULT;
  lFaceRegions: TList<AFR_FSDK_FACEINPUT>;
  lAgeRes: ASAE_FSDK_AGERESULT;
  lGenderRes: ASGE_FSDK_GENDERRESULT;
  lAges: TArray<Integer>;
  lGenders: TArray<Integer>;
  lFaceInfo: TFaceBaseInfo;
  i, iFaces: Integer;
  iWidth, iHeight, iLineBytes: Integer;
  ArrFaceOrient: array of AFD_FSDK_OrientCode;
  ArrFaceRect: array of MRECT;
{$IFDEF DEBUG}
  T: Cardinal;
{$ENDIF}
begin
  Result := False;

  if AFaceInfos = nil then
    AFaceInfos := TList<TFaceBaseInfo>.Create;
  if AFaceModels = nil then
    AFaceModels := TFaceModels.Create;

  if FFaceDetectionEngine = nil then
    Exit;

  pImgData := nil;

{$IFDEF DEBUG}
  T := GetTickCount;
{$ENDIF}
  if not ReadBmp(ABitmap, pImgData, iWidth, iHeight, iLineBytes) then
    Exit;
{$IFDEF DEBUG}
  T := GetTickCount - T;
  DoLog('载入数据耗时：' + IntToStr(T));
{$ENDIF}
  offInput.u32PixelArrayFormat := ASVL_PAF_RGB24_B8G8R8;
  FillChar(offInput.pi32Pitch, SizeOf(offInput.pi32Pitch), 0);
  FillChar(offInput.ppu8Plane, SizeOf(offInput.ppu8Plane), 0);

  offInput.i32Width := iWidth;
  offInput.i32Height := iHeight;

  offInput.ppu8Plane[0] := IntPtr(pImgData);
  offInput.pi32Pitch[0] := iLineBytes;
  //人脸检测
{$IFDEF DEBUG}
  T := GetTickCount;
{$ENDIF}
  nRet := AFD_FSDK_StillImageFaceDetection(FFaceDetectionEngine, @offInput,
    pFaceRes);
{$IFDEF DEBUG}
  DoLog('检测人脸耗时：' + IntToStr(GetTickCount - T));
{$ENDIF}
  if nRet = MOK then
  begin

    Result := true;

    lFaceRegions := TList<AFR_FSDK_FACEINPUT>.Create;
    try

      ExtractFaceBoxs(pFaceRes^, lFaceRegions);

      if lFaceRegions.Count > 0 then
      begin

        iFaces := lFaceRegions.Count;
        SetLength(ArrFaceOrient, iFaces);
        SetLength(ArrFaceRect, iFaces);
        for i := 0 to iFaces - 1 do
        begin
          ArrFaceOrient[i] := lFaceRegions.Items[i].lOrient;
          ArrFaceRect[i] := lFaceRegions.Items[i].rcFace;
        end;
        //===================================================
        //检测年龄
        //===================================================
        if (FFaceAgeEngine <> nil) then
        begin
          with lFaceRes_Age do
          begin
            pFaceRectArray := @ArrFaceRect[0];
            pFaceOrientArray := @ArrFaceOrient[0];
            lFaceNumber := iFaces;
          end;

{$IFDEF DEBUG}
          T := GetTickCount;
{$ENDIF}
          if ASAE_FSDK_AgeEstimation_StaticImage(
            FFaceAgeEngine, //[in] age estimation engine
            @offInput, //[in] the original image information
            //[in] the face rectangles information
            @lFaceRes_Age,
            //[out] the results of age estimation
            lAgeRes
            ) = MOK then
            //分解人脸年龄
            ExtractFaceAges(lAgeRes, lAges)
          else
            Result := False;
{$IFDEF DEBUG}
          DoLog('检测年龄耗时：' + IntToStr(GetTickCount - T));
{$ENDIF}
        end
        else
          Result := False;

        //===================================================
        //检测性别
        //===================================================
        if (FFaceGenderEngine <> nil) then
        begin
          with lFaceRes_Gender do
          begin
            pFaceRectArray := @ArrFaceRect[0];
            pFaceOrientArray := @ArrFaceOrient[0];
            lFaceNumber := iFaces;
          end;

{$IFDEF DEBUG}
          T := GetTickCount;
{$ENDIF}
          if ASGE_FSDK_GenderEstimation_StaticImage(
            FFaceGenderEngine, //[in] Gender estimation engine
            @offInput, //[in] the original imGender information
            //[in] the face rectangles information
            @lFaceRes_Gender,
            //[out] the results of Gender estimation
            lGenderRes
            ) = MOK then
            //分解人脸性别
            ExtractFaceGenders(lGenderRes, lGenders)
          else
            Result := False;
{$IFDEF DEBUG}
          DoLog('检测性别耗时：' + IntToStr(GetTickCount - T));
{$ENDIF}
        end
        else
          Result := False;

        for i := 0 to iFaces - 1 do
        begin
          lFaceInfo.Init;
          lFaceInfo.FaceRect := ArrFaceRect[i];
          lFaceInfo.FaceOrient := ArrFaceOrient[i];
          if i < Length(lAges) then
            lFaceInfo.Age := lAges[i];
          if i < Length(lGenders) then
            lFaceInfo.Gender := lGenders[i];
          AFaceInfos.Add(lFaceInfo);
        end;


        //===================================================
        //提取特征
        //===================================================

{$IFDEF DEBUG}
        T := GetTickCount;
{$ENDIF}
        if not ExtractFaceFeatures(offInput, lFaceRegions, AFaceModels) then
          Result := False;
{$IFDEF DEBUG}
        DoLog('提取特征耗时：' + IntToStr(GetTickCount - T));
{$ENDIF}
      end;

    finally
      lFaceRegions.Free;
    end;
  end;

  if pImgData <> nil then
    FreeMem(pImgData);

end;


{-------------------------------------------------------------------------------
  过程名:    TArcFaceSDK.DRAGfromBmpFile
  功能:      从BMP文件中获取人脸位置、年龄、性别和特征信息列表
  作者:      Bird
  日期:      2018.01.31
  参数:      AFileName: string; var AFaceInfos: TList<TFaceBaseInfo>; var AFaceModels: TFaceModels
  返回值:    Boolean
-------------------------------------------------------------------------------}
function TArcFaceSDK.DRAGfromBmpFile(AFileName: string; var AFaceInfos:
  TList<TFaceBaseInfo>; var AFaceModels: TFaceModels): Boolean;
var
  lBitmap: TBitmap;
begin
  Result := False;
  if not FileExists(AFileName) then
    Exit;

  lBitmap := TBitmap.Create;
  try
    if ReadBmpFile(AFileName, lBitmap) then
      Result := DRAGfromBmp(lBitmap, AFaceInfos, AFaceModels);
  finally
    lBitmap.Free;
  end;
end;


{-------------------------------------------------------------------------------
  过程名:    TArcFaceSDK.DrawFaceRectAgeGender
  功能:      在Canvas上画人脸框、年龄、性别
  作者:      Bird
  日期:      2018.01.31
  参数:      ACanvas: TCanvas; AFaceIdx: Integer; AFaceInfo: TFaceBaseInfo; AColor: TColor = clBlue; AWidth: Integer = 2; ADrawIndex: Boolean = true; ATextSize: Integer = 0
  返回值:    无
-------------------------------------------------------------------------------}
class procedure TArcFaceSDK.DrawFaceRectAgeGender(ACanvas: TCanvas; AFaceIdx:
  Integer; AFaceInfo: TFaceBaseInfo; AColor: TColor = clBlue; AWidth: Integer
  = 2; ADrawIndex: Boolean = true; ATextSize: Integer = 0);
var
  sText: string;
  iTextHeight, iTextWidth: Integer;
begin
  ATextSize := 0;
  ACanvas.Pen.Color := AColor;
  ACanvas.Pen.Width := AWidth;
  ACanvas.Brush.Style := bsClear;
  if ATextSize = 0 then
    ACanvas.Font.Size :=
      Round((AFaceInfo.FaceRect.bottom - AFaceInfo.FaceRect.top) / (10 * 1.5))
  else
    ACanvas.Font.Size := ATextSize;
  ACanvas.Font.Color := AColor;
  ACanvas.RoundRect(AFaceInfo.FaceRect.left,
    AFaceInfo.FaceRect.top,
    AFaceInfo.FaceRect.right, AFaceInfo.FaceRect.bottom, 0, 0);

  sText := '';
  case AFaceInfo.Gender of
    0:
      sText := '性别:男';
    1:
      sText := '性别:女';
  else
    sText := '性别:未知';
  end;

  sText := sText + ' 年龄:' + IntToStr(AFaceInfo.Age);

  if ADrawIndex then
  begin
    if AFaceIdx <> -1 then
      sText := IntToStr(AFaceIdx) + ' ' + sText;
  end;

  if sText <> '' then
  begin
    iTextWidth := ACanvas.TextWidth(sText);
    iTextHeight := ACanvas.TextHeight(sText);

    ACanvas.Brush.Style := bsSolid;
    ACanvas.Brush.Color := AColor;
    if ACanvas.Brush.Color = clWhite then
      ACanvas.Font.Color := clBlack
    else
      ACanvas.Font.Color := clWhite;
    ACanvas.Font.Quality := fqClearType;

    ACanvas.RoundRect(AFaceInfo.FaceRect.left,
      AFaceInfo.FaceRect.top - iTextHeight - 6,
      Max(AFaceInfo.FaceRect.right,
      AFaceInfo.FaceRect.left + iTextWidth + 10),
      AFaceInfo.FaceRect.top, 0, 0);

    ACanvas.TextOut(AFaceInfo.FaceRect.left + 5,
      AFaceInfo.FaceRect.top - 3 - iTextHeight, sText);
  end;

  ACanvas.Refresh;

end;


{-------------------------------------------------------------------------------
  过程名:    TArcFaceSDK.DrawFaceRect
  功能:      在Canvas上画人脸框
  作者:      Bird
  日期:      2018.01.31
  参数:      ACanvas: TCanvas; AFaceIdx: Integer; AFaceInfo: AFR_FSDK_FACEINPUT; AColor: TColor = clBlue; AWidth: Integer = 2; ADrawIndex: Boolean = true; ATextSize: Integer = 0
  返回值:    无
-------------------------------------------------------------------------------}
class procedure TArcFaceSDK.DrawFaceRect(ACanvas: TCanvas; AFaceIdx: Integer;
  AFaceInfo: AFR_FSDK_FACEINPUT; AColor: TColor = clBlue; AWidth: Integer =
  2; ADrawIndex: Boolean = true; ATextSize: Integer = 0);
var
  sText: string;
  iTextHeight, iTextWidth: Integer;
begin
  ATextSize := 0;
  ACanvas.Pen.Color := AColor;
  ACanvas.Pen.Width := AWidth;
  ACanvas.Brush.Style := bsClear;
  if ATextSize = 0 then
    ACanvas.Font.Size :=
      Round((AFaceInfo.rcFace.bottom - AFaceInfo.rcFace.top) / (10 * 1.5))
  else
    ACanvas.Font.Size := ATextSize;
  ACanvas.Font.Color := AColor;
  ACanvas.RoundRect(AFaceInfo.rcFace.left,
    AFaceInfo.rcFace.top,
    AFaceInfo.rcFace.right, AFaceInfo.rcFace.bottom, 0, 0);

  sText := '';
  if ADrawIndex then
  begin
    if AFaceIdx <> -1 then
      sText := IntToStr(AFaceIdx);
  end;

  if sText <> '' then
  begin
    iTextWidth := ACanvas.TextWidth(sText);
    iTextHeight := ACanvas.TextHeight(sText);

    ACanvas.Brush.Style := bsSolid;
    ACanvas.Brush.Color := AColor;
    if ACanvas.Brush.Color = clWhite then
      ACanvas.Font.Color := clBlack
    else
      ACanvas.Font.Color := clWhite;
    ACanvas.Font.Quality := fqClearType;

    ACanvas.RoundRect(AFaceInfo.rcFace.left,
      AFaceInfo.rcFace.top - iTextHeight - 6,
      Max(AFaceInfo.rcFace.right,
      AFaceInfo.rcFace.left + iTextWidth + 10),
      AFaceInfo.rcFace.top, 0, 0);

    ACanvas.TextOut(Round((AFaceInfo.rcFace.left - iTextWidth) /
      2), AFaceInfo.rcFace.top - 3 - iTextHeight, sText);

  end;

  ACanvas.Refresh;

end;

{-------------------------------------------------------------------------------
 过程名:    TArcFaceSDK.ExtractFaceBoxs
 功能:      提取API人脸检测结果列表到Delphi泛型列表(跟踪模式)
 作者:      Bird
 日期:      2018.01.19
 参数:      AFaces: AFT_FSDK_FACERES;//人脸位置框原始数据
 var AFaceRegions: TList<AFR_FSDK_FACEINPUT> //分解输出列表
 返回值:    无
 -------------------------------------------------------------------------------}
class procedure TArcFaceSDK.ExtractFaceBoxs(AFaces: AFT_FSDK_FACERES; var
  AFaceRegions: TList<AFR_FSDK_FACEINPUT>);
var
  i, j: Integer;
  lFace: AFR_FSDK_FACEINPUT;
begin
  //if AFaceRegions = nil then
  //AFaceRegions := TList<AFR_FSDK_FACEINPUT>.Create;
  j := SizeOf(Integer);
  for i := 0 to AFaces.nFace - 1 do
  begin
    lFace.rcFace := pmrect(AFaces.rcFace + i * SizeOf(MRECT))^;
    //if AFaces.lfaceOrient < 100 then
    lFace.lOrient := AFaces.lfaceOrient;
    //else
    //lFace.lOrient := Pint(AFaces.lfaceOrient + i * j)^;
    AFaceRegions.Add(lFace);
  end;
end;

{-------------------------------------------------------------------------------
 过程名:    TArcFaceSDK.ExtractFaceBoxs
 功能:      提取API人脸检测结果列表到Delphi泛型列表
 作者:      Bird
 日期:      2017.11.19
 参数:      AFaces: AFD_FSDK_FACERES; //人脸位置框原始数据
 var AFaceRegions: TList<AFR_FSDK_FACEINPUT>//分解输出列表
 返回值:    无
 -------------------------------------------------------------------------------}
class procedure TArcFaceSDK.ExtractFaceBoxs(AFaces: AFD_FSDK_FACERES;
  AFaceRegions: TList<AFR_FSDK_FACEINPUT>);
var
  i, j: Integer;
  lFace: AFR_FSDK_FACEINPUT;
begin
  //if AFaceRegions = nil then
  //AFaceRegions := TList<AFR_FSDK_FACEINPUT>.Create;
  j := SizeOf(AFD_FSDK_OrientCode);
  for i := 0 to AFaces.nFace - 1 do
  begin
    lFace.rcFace := pmrect(AFaces.rcFace + i * SizeOf(MRECT))^;
    lFace.lOrient := Pint(AFaces.lfaceOrient + i * j)^;
    AFaceRegions.Add(lFace);
  end;
end;

{-------------------------------------------------------------------------------
 过程名:    TArcFaceSDK.ExtractFaceAges
 功能:      提取API人脸年龄检测结果列表到数组
 作者:      Bird
 日期:      2017.12.17
 参数:      AFaceAgeResults: ASAE_FSDK_AGERESULT; //年齡检测结果
 var AFaceAges: TArray<Integer> //输出整形数组
 返回值:    无
 -------------------------------------------------------------------------------}
class procedure TArcFaceSDK.ExtractFaceAges(AFaceAgeResults:
  ASAE_FSDK_AGERESULT; var AFaceAges: TArray<Integer>);
var
  i, j: Integer;
begin
  j := SizeOf(MInt32);
  SetLength(AFaceAges, AFaceAgeResults.lFaceNumber);
  for i := 0 to AFaceAgeResults.lFaceNumber - 1 do
    AFaceAges[i] := Pint(AFaceAgeResults.pAgeResultArray + i * j)^;
end;

{-------------------------------------------------------------------------------
 过程名:    TArcFaceSDK.ExtractFaceAges
 功能:      提取API人脸性别检测结果列表到数组
 作者:      Bird
 日期:      2017.12.17
 参数:      AFaceGenderResults: ASGE_FSDK_GENDERRESULT; //性别检测结果
 var AFaceGenders: TArray<Integer> //输出整形数组
 返回值:    无
 -------------------------------------------------------------------------------}
class procedure TArcFaceSDK.ExtractFaceGenders(AFaceGenderResults:
  ASGE_FSDK_GENDERRESULT; var AFaceGenders: TArray<Integer>);
var
  i, j: Integer;
begin
  j := SizeOf(Integer);
  SetLength(AFaceGenders, AFaceGenderResults.lFaceNumber);
  for i := 0 to AFaceGenderResults.lFaceNumber - 1 do
  begin
    AFaceGenders[i] := Pint(AFaceGenderResults.pGenderResultArray + (i * j))^;
  end;
end;

{-------------------------------------------------------------------------------
 过程名:    TArcFaceSDK.ExtractFaceFeature
 功能:      根据给定的单个人脸框提取单个人脸特征
 作者:      Bird
 日期:      2017.09.24
 参数:      AFaceInput: ASVLOFFSCREEN; //图片数据
 AFaceRegion: AFR_FSDK_FACEINPUT; //人脸位置信息
 var AFaceModel: AFR_FSDK_FACEMODEL //人脸特征，特征数据内存需手动使用freemem释放
 返回值:    Boolean
 -------------------------------------------------------------------------------}
function TArcFaceSDK.ExtractFaceFeature(AFaceInput: ASVLOFFSCREEN; AFaceRegion:
  AFR_FSDK_FACEINPUT; var AFaceModel: AFR_FSDK_FACEMODEL): Boolean;
var
  tmpFaceModels: AFR_FSDK_FACEMODEL;
begin
  Result := False;
  if FFaceRecognitionEngine = nil then
    Exit;

  with AFaceModel do
  begin
    pbFeature := nil; //The extracted features
    lFeatureSize := 0;
  end;

  with tmpFaceModels do
  begin
    pbFeature := nil; //The extracted features
    lFeatureSize := 0;
  end;

  //提取人脸特征
  Result := AFR_FSDK_ExtractFRFeature(FFaceRecognitionEngine, @AFaceInput,
    @AFaceRegion, tmpFaceModels) = MOK;
  if Result then
  begin
    AFaceModel.lFeatureSize := tmpFaceModels.lFeatureSize;
    GetMem(AFaceModel.pbFeature, AFaceModel.lFeatureSize);
    CopyMemory(AFaceModel.pbFeature, tmpFaceModels.pbFeature,
      AFaceModel.lFeatureSize);
  end;

end;

{-------------------------------------------------------------------------------
 过程名:    TArcFaceSDK.ExtractFaceFeatures
 功能:      根据给定的多个人脸框提取多个人脸特征
 作者:      Bird
 日期:      2017.09.24
 参数:      AFaceInput: ASVLOFFSCREEN; //图片数据
 AFaceRegions: TList<AFR_FSDK_FACEINPUT>; //人脸位置信息信息列表
 var AFaceModels: TFaceModels //输出人脸特征列表
 返回值:    Boolean
 -------------------------------------------------------------------------------}
function TArcFaceSDK.ExtractFaceFeatures(AFaceInput: ASVLOFFSCREEN;
  AFaceRegions: TList<AFR_FSDK_FACEINPUT>; var AFaceModels: TFaceModels):
  Boolean;
var
  lFaceModel: AFR_FSDK_FACEMODEL;
  i: Integer;
begin
  Result := False;
  if FFaceRecognitionEngine = nil then
    Exit;

  if AFaceModels = nil then
    AFaceModels := TFaceModels.Create;

  for i := 0 to AFaceRegions.Count - 1 do
    if ExtractFaceFeature(AFaceInput, AFaceRegions.Items[i], lFaceModel) then
      AFaceModels.AddModel(lFaceModel);
  Result := true;

end;

{-------------------------------------------------------------------------------
 过程名:    TArcFaceSDK.ExtractFaceFeatureFromBmp
 功能:      从Bitmap中提取单个人脸特征
 作者:      Bird
 日期:      2017.09.24
 参数:      ABitmap: TBitmap; //Bitmap数据
 AFaceRegion: AFR_FSDK_FACEINPUT; //人脸位置信息
 var AFaceModel: AFR_FSDK_FACEMODEL //人脸特征，特征数据内存需手动使用freemem释放
 返回值:    Boolean
 -------------------------------------------------------------------------------}
function TArcFaceSDK.ExtractFaceFeatureFromBmp(ABitmap: TBitmap; AFaceRegion:
  AFR_FSDK_FACEINPUT; var AFaceModel: AFR_FSDK_FACEMODEL): Boolean;
var
  pImgData: PByte;
  iWidth, iHeight, iLineBytes: Integer;
  offInput: ASVLOFFSCREEN;
  pFaceRes: LPAFD_FSDK_FACERES;
begin
  Result := False;

  if FFaceRecognitionEngine = nil then
    Exit;

  pImgData := nil;

  if not ReadBmp(ABitmap, pImgData, iWidth, iHeight, iLineBytes) then
    Exit;

  offInput.u32PixelArrayFormat := ASVL_PAF_RGB24_B8G8R8;
  FillChar(offInput.pi32Pitch, SizeOf(offInput.pi32Pitch), 0);
  FillChar(offInput.ppu8Plane, SizeOf(offInput.ppu8Plane), 0);
  offInput.i32Width := iWidth;
  offInput.i32Height := iHeight;

  offInput.ppu8Plane[0] := IntPtr(pImgData);
  offInput.pi32Pitch[0] := iLineBytes;

  //人脸特征提取
  Result := ExtractFaceFeature(offInput, AFaceRegion, AFaceModel);

  if pImgData <> nil then
    FreeMem(pImgData);

end;

{-------------------------------------------------------------------------------
 过程名:    TArcFaceSDK.ExtractFaceFeaturesFromBmp
 功能:      从Bitmap中提取多个人脸特征
 作者:      Bird
 日期:      2017.09.24
 参数:      ABitmap: TBitmap; //图片数据
 AFaceRegions: TList<AFR_FSDK_FACEINPUT>; //人脸位置信息信息列表
 var AFaceModels: TFaceModels //输出人脸特征列表
 返回值:    Boolean
 -------------------------------------------------------------------------------}
function TArcFaceSDK.ExtractFaceFeaturesFromBmp(ABitmap: TBitmap; AFaceRegions:
  TList<AFR_FSDK_FACEINPUT>; var AFaceModels: TFaceModels): Boolean;
var
  pImgData: PByte;
  iWidth, iHeight, iLineBytes: Integer;
  offInput: ASVLOFFSCREEN;
  pFaceRes: LPAFD_FSDK_FACERES;
begin
  Result := False;

  if FFaceRecognitionEngine = nil then
    Exit;

  pImgData := nil;

  if not ReadBmp(ABitmap, pImgData, iWidth, iHeight, iLineBytes) then
    Exit;

  offInput.u32PixelArrayFormat := ASVL_PAF_RGB24_B8G8R8;
  FillChar(offInput.pi32Pitch, SizeOf(offInput.pi32Pitch), 0);
  FillChar(offInput.ppu8Plane, SizeOf(offInput.ppu8Plane), 0);
  offInput.i32Width := iWidth;
  offInput.i32Height := iHeight;

  offInput.ppu8Plane[0] := IntPtr(pImgData);
  offInput.pi32Pitch[0] := iLineBytes;

  //人脸特征提取
  Result := ExtractFaceFeatures(offInput, AFaceRegions, AFaceModels);

  if pImgData <> nil then
    FreeMem(pImgData);

end;

{-------------------------------------------------------------------------------
 过程名:    TArcFaceSDK.ExtractFaceFeatureFromBmpFile
 功能:      从BMP文件中提取单个人脸特征
 作者:      Bird
 日期:      2017.09.24
 参数:
 AFile: string; //BMP图片文件，请确保文件格式为BMP
 AFaceRegion: AFR_FSDK_FACEINPUT; //人脸位置信息
 var AFaceModel: AFR_FSDK_FACEMODEL //人脸特征，特征数据内存需手动使用freemem释放
 返回值:    Boolean
 -------------------------------------------------------------------------------}
function TArcFaceSDK.ExtractFaceFeatureFromBmpFile(AFile: string; AFaceRegion:
  AFR_FSDK_FACEINPUT; var AFaceModel: AFR_FSDK_FACEMODEL): Boolean;
var
  pImgData: PByte;
  iWidth, iHeight, iLineBytes: Integer;
  offInput: ASVLOFFSCREEN;
  pFaceRes: LPAFD_FSDK_FACERES;
begin
  Result := False;

  if FFaceRecognitionEngine = nil then
    Exit;

  pImgData := nil;

  if not ReadBmpFile(AFile, pImgData, iWidth, iHeight, iLineBytes) then
    Exit;

  offInput.u32PixelArrayFormat := ASVL_PAF_RGB24_B8G8R8;
  FillChar(offInput.pi32Pitch, SizeOf(offInput.pi32Pitch), 0);
  FillChar(offInput.ppu8Plane, SizeOf(offInput.ppu8Plane), 0);
  offInput.i32Width := iWidth;
  offInput.i32Height := iHeight;

  offInput.ppu8Plane[0] := IntPtr(pImgData);
  offInput.pi32Pitch[0] := iLineBytes;

  //人脸特征提取
  Result := ExtractFaceFeature(offInput, AFaceRegion, AFaceModel);

  if pImgData <> nil then
    FreeMem(pImgData);

end;

{-------------------------------------------------------------------------------
 过程名:    TArcFaceSDK.ExtractFaceFeaturesFromBmpFile
 功能:      从BMP文件中提取多个人脸特征
 作者:      Bird
 日期:      2017.09.24
 参数:
 AFile: string; //BMP图片文件
 AFaceRegions: TList<AFR_FSDK_FACEINPUT>; //人脸位置信息信息列表
 var AFaceModels: TFaceModels //人脸特征列表
 返回值:    Boolean
 -------------------------------------------------------------------------------}
function TArcFaceSDK.ExtractFaceFeaturesFromBmpFile(AFile: string;
  AFaceRegions: TList<AFR_FSDK_FACEINPUT>; var AFaceModels: TFaceModels):
  Boolean;
var
  pImgData: PByte;
  iWidth, iHeight, iLineBytes: Integer;
  offInput: ASVLOFFSCREEN;
  pFaceRes: LPAFD_FSDK_FACERES;
begin
  Result := False;

  if FFaceRecognitionEngine = nil then
    Exit;

  pImgData := nil;

  if not ReadBmpFile(AFile, pImgData, iWidth, iHeight, iLineBytes) then
    Exit;

  offInput.u32PixelArrayFormat := ASVL_PAF_RGB24_B8G8R8;
  FillChar(offInput.pi32Pitch, SizeOf(offInput.pi32Pitch), 0);
  FillChar(offInput.ppu8Plane, SizeOf(offInput.ppu8Plane), 0);
  offInput.i32Width := iWidth;
  offInput.i32Height := iHeight;

  offInput.ppu8Plane[0] := IntPtr(pImgData);
  offInput.pi32Pitch[0] := iLineBytes;

  //人脸特征提取
  Result := ExtractFaceFeatures(offInput, AFaceRegions, AFaceModels);

  if pImgData <> nil then
    FreeMem(pImgData);

end;

{-------------------------------------------------------------------------------
 过程名:    TArcFaceSDK.InitialFaceDetectionEngine
 功能:      初始化人脸检测引擎
 作者:      Bird
 日期:      2017.11.19
 参数:      Deinitial: Boolean
 返回值:    Integer
 -------------------------------------------------------------------------------}
function TArcFaceSDK.InitialFaceDetectionEngine(Deinitial: Boolean): Integer;
begin

  if FFaceDetectionEngine <> nil then
  begin
    if Deinitial then
    begin
      //释放引擎
      Result := UnInitialFaceDetectionEngine;
      if Result <> MOK then
        Exit;
    end
    else
    begin
      Result := MOK;
      Exit;
    end;
  end;

  GetMem(FpFaceDetectionBuf, FWorkKBufferSize);
  //初始化引擎
  Result := AFD_FSDK_InitialFaceEngine(
    pansichar(AnsiString(FAppID)), //[in]  APPID
    pansichar(AnsiString(FFaceDetectionKey)), //[in]  SDKKEY
    FpFaceDetectionBuf, //[in]	 User allocated memory for the engine
    FWorkKBufferSize, //WORKBUF_SIZE, //[in]	 User allocated memory size
    FFaceDetectionEngine, //[out] Pointing to the detection engine.
    //[in]  Defining the priority of face orientation
    ord(FOrientPriority),
    //[in]  An integer defining the minimal face to detect relative to the maximum of image width and height.
    FScale,
    //[in]  An integer defining the number of max faces to detection
    FMaxFace
    );

  if Result <> MOK then
  begin
    FreeMem(FpFaceDetectionBuf);
    FpFaceDetectionBuf := nil;
  end;

end;

{-------------------------------------------------------------------------------
 过程名:    TArcFaceSDK.InitialFaceTrackingEngine
 功能:      初始化人脸追踪引擎
 作者:      Bird
 日期:      2017.11.19
 参数:      Deinitial: Boolean
 返回值:    Integer
 -------------------------------------------------------------------------------}
function TArcFaceSDK.InitialFaceTrackingEngine(Deinitial: Boolean): Integer;
begin

  if FFaceTrackingEngine <> nil then
  begin
    if Deinitial then
    begin
      Result := UnInitialFaceTrackingEngine;
      if Result <> MOK then
        Exit;
    end
    else
    begin
      Result := MOK;
      Exit;
    end;
  end;

  GetMem(FpFaceTrackingBuf, FWorkKBufferSize);
  //初始化
  Result := AFT_FSDK_InitialFaceEngine(
    pansichar(AnsiString(FAppID)), //[in]  APPID
    pansichar(AnsiString(FFaceTrackingKey)), //[in]  SDKKEY
    FpFaceTrackingBuf, //[in]	 User allocated memory for the engine
    FWorkKBufferSize, //WORKBUF_SIZE, //[in]	 User allocated memory size
    FFaceTrackingEngine, //[out] Pointing to the Tracking engine.
    //[in]  Defining the priority of face orientation
    ord(FOrientPriority),
    //[in]  An integer defining the minimal face to detect relative to the maximum of image width and height.
    FScale,
    //[in]  An integer defining the number of max faces to Tracking
    FMaxFace
    );
  if Result <> MOK then
  begin
    FreeMem(FpFaceTrackingBuf);
    FpFaceTrackingBuf := nil;
  end;

end;

{-------------------------------------------------------------------------------
 过程名:    TArcFaceSDK.InitialFaceRecognitionEngine
 功能:      初始化人脸特征检测引擎
 作者:      Bird
 日期:      2017.11.19
 参数:      Deinitial: Boolean
 返回值:    Integer
 -------------------------------------------------------------------------------}
function TArcFaceSDK.InitialFaceRecognitionEngine(Deinitial: Boolean): Integer;
begin

  if FFaceRecognitionEngine <> nil then
  begin
    if Deinitial then
    begin
      Result := UnInitialFaceRecognitionEngine;
      if Result <> MOK then
        Exit;
    end
    else
    begin
      Result := MOK;
      Exit;
    end;
  end;

  GetMem(FpFaceRecognitionBuf, FWorkKBufferSize);
  Result := AFR_FSDK_InitialEngine(
    pansichar(AnsiString(FAppID)), //[in]  APPID
    pansichar(AnsiString(FFaceRecognitionKey)), //[in]  SDKKEY
    FpFaceRecognitionBuf, //[in]	 User allocated memory for the engine
    FWorkKBufferSize, //WORKBUF_SIZE, //[in]	 User allocated memory size
    FFaceRecognitionEngine
    );

  if Result <> MOK then
  begin
    FreeMem(FpFaceRecognitionBuf);
    FpFaceRecognitionBuf := nil;
  end;

end;

{-------------------------------------------------------------------------------
 过程名:    TArcFaceSDK.InitialFaceAgeEngine
 功能:      初始化人脸年龄检测引擎
 作者:      Bird
 日期:      2017.11.19
 参数:      Deinitial: Boolean
 返回值:    Integer
 -------------------------------------------------------------------------------}
function TArcFaceSDK.InitialFaceAgeEngine(Deinitial: Boolean): Integer;
begin

  if FFaceAgeEngine <> nil then
  begin
    if Deinitial then
    begin
      Result := UnInitialFaceAgeEngine;
      if Result <> MOK then
        Exit;
    end
    else
    begin
      Result := MOK;
      Exit;
    end;
  end;

  GetMem(FpFaceAgeBuf, FEstimationBufferSize);
  Result := ASAE_FSDK_InitAgeEngine(
    pansichar(AnsiString(FAppID)), //[in]  APPID
    pansichar(AnsiString(FFaceAgeKey)), //[in]  SDKKEY
    FpFaceAgeBuf, //[in]	 User allocated memory for the engine
    FEstimationBufferSize, //WORKBUF_SIZE, //[in]	 User allocated memory size
    FFaceAgeEngine
    );

  if Result <> MOK then
  begin
    FreeMem(FpFaceAgeBuf);
    FpFaceAgeBuf := nil;
  end;

end;

{-------------------------------------------------------------------------------
 过程名:    TArcFaceSDK.InitialFaceGenderEngine
 功能:      初始化人脸性别检测引擎
 作者:      Bird
 日期:      2017.11.19
 参数:      Deinitial: Boolean
 返回值:    Integer
 -------------------------------------------------------------------------------}
function TArcFaceSDK.InitialFaceGenderEngine(Deinitial: Boolean): Integer;
begin

  if FFaceGenderEngine <> nil then
  begin
    if Deinitial then
    begin
      Result := UnInitialFaceGenderEngine;
      if Result <> MOK then
        Exit;
    end
    else
    begin
      Result := MOK;
      Exit;
    end;
  end;

  GetMem(FpFaceGenderBuf, FEstimationBufferSize);
  Result := ASGE_FSDK_InitGenderEngine(
    pansichar(AnsiString(FAppID)), //[in]  APPID
    pansichar(AnsiString(FFaceGenderKey)), //[in]  SDKKEY
    FpFaceGenderBuf, //[in]	 User allocated memory for the engine
    FEstimationBufferSize, //WORKBUF_SIZE, //[in]	 User allocated memory size
    FFaceGenderEngine
    );

  if Result <> MOK then
  begin
    FreeMem(FpFaceGenderBuf);
    FpFaceGenderBuf := nil;
  end;

end;

{-------------------------------------------------------------------------------
 过程名:    TArcFaceSDK.MatchFace
 功能:      比对两个人脸特征
 作者:      Bird
 日期:      2017.11.19
 参数:      AFaceModel1, AFaceModel2: AFR_FSDK_FACEMODEL
 返回值:    Single
 -------------------------------------------------------------------------------}
function TArcFaceSDK.MatchFace(AFaceModel1, AFaceModel2: AFR_FSDK_FACEMODEL):
  Single;
var
  fSimilScore: MFloat;
begin
  Result := 0;
  if FFaceRecognitionEngine = nil then
    Exit;

  //对比两张人脸特征，获得比对结果
  fSimilScore := 0.0;
  if AFR_FSDK_FacePairMatching(FFaceRecognitionEngine, @AFaceModel1,
    @AFaceModel2, fSimilScore) = MOK then
    Result := fSimilScore;
end;

{-------------------------------------------------------------------------------
 过程名:    TArcFaceSDK.MatchFaceWithBitmaps
 功能:      比对两张图像（只取两张图像的第一个人脸进行比对）
 作者:      Bird
 日期:      2017.11.19
 参数:      ABitmap1, ABitmap2: TBitmap
 返回值:    Single
 -------------------------------------------------------------------------------}
function TArcFaceSDK.MatchFaceWithBitmaps(ABitmap1, ABitmap2: TBitmap): Single;
var
  AFaceRegions1, AFaceRegions2: TList<AFR_FSDK_FACEINPUT>;
  AFaceModels1, AFaceModels2: TFaceModels;
  i: Integer;
  T: Cardinal;
begin
  Result := 0;
  if (ABitmap1 = nil) or (ABitmap2 = nil) then
    Exit;

  AFaceRegions1 := TList<AFR_FSDK_FACEINPUT>.Create;
  AFaceRegions2 := TList<AFR_FSDK_FACEINPUT>.Create;
  AFaceModels1 := TFaceModels.Create;
  AFaceModels2 := TFaceModels.Create;
  try
{$IFDEF DEBUG}
    T := GetTickCount;
{$ENDIF}
    DetectAndRecognitionFacesFromBmp(ABitmap1, AFaceRegions1,
      AFaceModels1);
{$IFDEF DEBUG}
    T := GetTickCount - T;
    DoLog('取图一特征耗时：' + IntToStr(T));
{$ENDIF}

{$IFDEF DEBUG}
    T := GetTickCount;
{$ENDIF}
    DetectAndRecognitionFacesFromBmp(ABitmap2, AFaceRegions2,
      AFaceModels2);
{$IFDEF DEBUG}
    T := GetTickCount - T;
    DoLog('取图二特征耗时：' + IntToStr(T));
{$ENDIF}
    if (AFaceModels1.Count > 0) and (AFaceModels2.Count > 0) then
    begin
{$IFDEF DEBUG}
      T := GetTickCount;
{$ENDIF}
      Result := MatchFace(AFaceModels1.FaceModel[0],
        AFaceModels2.FaceModel[0]);
{$IFDEF DEBUG}
      T := GetTickCount - T;
      DoLog('比对耗时：' + IntToStr(T));
{$ENDIF}
    end;

  finally
    AFaceRegions1.Free;
    AFaceRegions2.Free;
    AFaceModels1.Free;
    AFaceModels2.Free;
  end;

end;

{-------------------------------------------------------------------------------
 过程名:    TArcFaceSDK.UnInitialFaceDetectionEngine
 功能:      释放人脸检测引擎
 作者:      Bird
 日期:      2017.09.24
 参数:      无
 返回值:    Integer
 -------------------------------------------------------------------------------}
function TArcFaceSDK.UnInitialFaceDetectionEngine: Integer;
begin

  if FFaceDetectionEngine <> nil then
  begin
    Result := AFD_FSDK_UninitialFaceEngine(FFaceDetectionEngine);
    if Result = MOK then
      FFaceDetectionEngine := nil;
  end
  else
    Result := MOK;

  if FpFaceDetectionBuf <> nil then
  begin
    FreeMem(FpFaceDetectionBuf);
    FpFaceDetectionBuf := nil;
  end;

end;

{-------------------------------------------------------------------------------
 过程名:    TArcFaceSDK.ReadBmp
 功能:      读取Bitmap到内存
 作者:      Bird
 日期:      2017.11.19
 参数:      ABitmap: TBitmap; var AImgData: PByte; var AWidth, AHeight, ALineBytes: Integer
 返回值:    Boolean
 -------------------------------------------------------------------------------}
class function TArcFaceSDK.ReadBmp(ABitmap: TBitmap; var AImgData: PByte; var
  AWidth, AHeight, ALineBytes: Integer): Boolean;
var
  iLineByte: Integer;
  iBitCount: Integer;
  i: Integer;
  //获取位深
  function GetBitCount: Integer;
  begin
    case ABitmap.PixelFormat of
      pf1bit:
        Result := 1;
      pf4bit:
        Result := 4;
      pf8bit:
        Result := 8;
      pf15bit:
        Result := 16;
      pf16bit:
        Result := 16;
      pf24bit:
        Result := 24;
      pf32bit:
        Result := 32;
    else
      Result := 0;
    end;
  end;

begin
  Result := False;
  iBitCount := GetBitCount;
  if iBitCount = 0 then
    Exit;

  AWidth := ABitmap.Width;
  AHeight := ABitmap.Height;

  //获取位图行长度
  iLineByte := Trunc((ABitmap.Width * iBitCount / 8 + 3) / 4) * 4;
  ALineBytes := iLineByte;

  GetMem(AImgData, iLineByte * ABitmap.Height);

  //读入内存，注意为倒序，从最后一行开始读
  for i := ABitmap.Height - 1 downto 0 do
  begin
    CopyMemory(Pointer(AImgData + i * iLineByte), ABitmap.ScanLine[i],
      iLineByte);
  end;

  Result := true;
end;

{-------------------------------------------------------------------------------
 过程名:    TArcFaceSDK.ReadBmpFile
 功能:      读取磁盘上的BMP文件到内存
 作者:      Bird
 日期:      2017.11.19
 参数:      AFilename: string; var AImgData: PByte; var AWidth, AHeight, ALineBytes: Integer
 返回值:    Boolean
 -------------------------------------------------------------------------------}
class function TArcFaceSDK.ReadBmpFile(AFileName: string; var AImgData: PByte;
  var AWidth, AHeight, ALineBytes: Integer): Boolean;
var
  lBitmap: TBitmap;
begin

  Result := False;
  if not FileExists(AFileName) then
    Exit;

  lBitmap := TBitmap.Create;
  try
    lBitmap.LoadFromFile(AFileName);
    Result := ReadBmp(lBitmap, AImgData, AWidth, AHeight, ALineBytes);
  finally
    lBitmap.Free;
  end;

end;

{-------------------------------------------------------------------------------
 过程名:    TArcFaceSDK.ReadBmpFile
 功能:      读取磁盘上的BMP文件到内存
 作者:      Bird
 日期:      2017.11.19
 参数:      AFilename: string; var ABitmap: PByte; var AWidth, AHeight, ALineBytes: Integer
 返回值:    Boolean
 -------------------------------------------------------------------------------}
class function TArcFaceSDK.ReadBmpFile(AFileName: string; ABitmap: TBitmap):
  Boolean;
begin

  Result := False;
  if not FileExists(AFileName) then
    Exit;

  ABitmap.LoadFromFile(AFileName);
  Result := true;

end;

{-------------------------------------------------------------------------------
 过程名:    TArcFaceSDK.ReadJpegFile
 功能:      读取磁盘上的JPG文件到内存并转换为TBitmap
 作者:      Bird
 日期:      2017.11.19
 参数:      AFilename: string; var AImgData: PByte; var AWidth, AHeight, ABitCount: Integer
 返回值:    Boolean
 -------------------------------------------------------------------------------}
class function TArcFaceSDK.ReadJpegFile(AFileName: string; var AImgData: PByte;
  var AWidth, AHeight, ABitCount: Integer): Boolean;
var
  lBitmap: TBitmap;
  lJpeg: TuJpegImage;
begin

  Result := False;
  if not FileExists(AFileName) then
    Exit;
  lJpeg := TuJpegImage.Create;

  try
    lJpeg.LoadFromFile(AFileName);
    lBitmap := lJpeg.BitmapData;
    Result := ReadBmp(lBitmap, AImgData, AWidth, AHeight, ABitCount);
  finally
    lBitmap := nil;
    lJpeg.Free;
  end;

end;

{-------------------------------------------------------------------------------
 过程名:    TArcFaceSDK.ReadBmpStream
 功能:      读取BMP流
 作者:      Bird
 日期:      2017.11.19
 参数:      AStream: TMemoryStream; var AImgData: PByte; var AWidth, AHeight, ALineBytes: Integer
 返回值:    Boolean
 -------------------------------------------------------------------------------}
class function TArcFaceSDK.ReadBmpStream(AStream: TMemoryStream; var AImgData:
  PByte; var AWidth, AHeight, ALineBytes: Integer): Boolean;
var
  lBitmap: TBitmap;
begin

  Result := False;
  if AStream = nil then
    Exit;

  lBitmap := TBitmap.Create;
  try
    lBitmap.LoadFromStream(AStream);
    Result := ReadBmp(lBitmap, AImgData, AWidth, AHeight, ALineBytes);
  finally
    lBitmap.Free;
  end;

end;

{-------------------------------------------------------------------------------
 过程名:    TArcFaceSDK.ReadJpegFile
 功能:      读取磁盘上的JPG文件到内存并转换为TBitmap
 作者:      Bird
 日期:      2017.11.19
 参数:      AFilename: string; var ABitmap: PByte; var AWidth, AHeight, ABitCount: Integer
 返回值:    Boolean
 -------------------------------------------------------------------------------}
class function TArcFaceSDK.ReadJpegFile(AFileName: string; ABitmap: TBitmap):
  Boolean;
var
  lJpeg: TuJpegImage;
begin

  Result := False;
  if not FileExists(AFileName) then
    Exit;
  lJpeg := TuJpegImage.Create;
  try
    lJpeg.LoadFromFile(AFileName);
    ABitmap.Assign(lJpeg.BitmapData);
    Result := true;
  finally
    lJpeg.Free;
  end;

end;

procedure TArcFaceSDK.SetMaxFace(const Value: Integer);
begin
  FMaxFace := Value;
end;

procedure TArcFaceSDK.SetScale(const Value: Integer);
begin
  FScale := Value;
end;

{-------------------------------------------------------------------------------
 过程名:    TArcFaceSDK.TrackFacesAndAgeGenderFromIEBitmap
 功能:      从IEBitmap中获取人脸位置、性别和年龄信息列表（追踪模式）
 作者:      DelphiDX10
 日期:      2018.01.31
 参数:      ABitmap: TIEBitmap; var AFaceInfos: TList<TFaceBaseInfo>; AUseCache: Boolean
 返回值:    Boolean
 -------------------------------------------------------------------------------}
function TArcFaceSDK.TrackFacesAndAgeGenderFromBmp(ABitmap: TBitmap; var
  AFaceInfos: TList<TFaceBaseInfo>): Boolean;
var
  pImgData: PByte;
  offInput: ASVLOFFSCREEN;
  pFaceRes: LPAFT_FSDK_FACERES;
  lFaceRes_Age: ASAE_FSDK_AGEFACEINPUT;
  lFaceRes_Gender: ASGE_FSDK_GENDERFACEINPUT;
  lFaceRegions: TList<AFR_FSDK_FACEINPUT>;
  lAgeRes: ASAE_FSDK_AGERESULT;
  lGenderRes: ASGE_FSDK_GENDERRESULT;
  lAges: TArray<Integer>;
  lGenders: TArray<Integer>;
  lFaceInfo: TFaceBaseInfo;
  i, iFaces: Integer;
  iWidth, iHeight, iLineBytes: Integer;
  ArrFaceOrient: array of AFT_FSDK_OrientCode;
  ArrFaceRect: array of MRECT;
  R: MRESULT;
begin
  Result := False;

  if AFaceInfos = nil then
    AFaceInfos := TList<TFaceBaseInfo>.Create;

  if FFaceDetectionEngine = nil then
    Exit;

  pImgData := nil;

  if not ReadBmp(ABitmap, pImgData, iWidth, iHeight, iLineBytes) then
    Exit;

  offInput.u32PixelArrayFormat := ASVL_PAF_RGB24_B8G8R8;
  FillChar(offInput.pi32Pitch, SizeOf(offInput.pi32Pitch), 0);
  FillChar(offInput.ppu8Plane, SizeOf(offInput.ppu8Plane), 0);

  offInput.i32Width := iWidth;
  offInput.i32Height := iHeight;

  offInput.ppu8Plane[0] := IntPtr(pImgData);
  offInput.pi32Pitch[0] := iLineBytes;

  lFaceRegions := TList<AFR_FSDK_FACEINPUT>.Create;
  try
    //人脸检测
    R := AFT_FSDK_FaceFeatureDetect(FFaceTrackingEngine, @offInput, pFaceRes);
    if R = MOK then
    begin
      //分解人脸位置信息
      ExtractFaceBoxs(pFaceRes^, lFaceRegions);
      if lFaceRegions.Count > 0 then
      begin
        iFaces := lFaceRegions.Count;
        SetLength(ArrFaceOrient, iFaces);
        SetLength(ArrFaceRect, iFaces);
        for i := 0 to iFaces - 1 do
        begin
          ArrFaceOrient[i] := lFaceRegions.Items[i].lOrient;
          ArrFaceRect[i] := lFaceRegions.Items[i].rcFace;
        end;

        //检测年龄
        if (FFaceAgeEngine <> nil) then
        begin
          with lFaceRes_Age do
          begin
            pFaceRectArray := @ArrFaceRect[0];
            pFaceOrientArray := @ArrFaceOrient[0];
            lFaceNumber := iFaces;
          end;

          if ASAE_FSDK_AgeEstimation_Preview(
            FFaceAgeEngine, //[in] age estimation engine
            @offInput, //[in] the original image information
            //[in] the face rectangles information
            @lFaceRes_Age,
            //[out] the results of age estimation
            lAgeRes
            ) = MOK then
            //分解人脸年龄
            ExtractFaceAges(lAgeRes, lAges);

        end;

        //检测性别
        if (FFaceGenderEngine <> nil) then
        begin
          with lFaceRes_Gender do
          begin
            pFaceRectArray := @ArrFaceRect[0];
            pFaceOrientArray := @ArrFaceOrient[0];
            lFaceNumber := iFaces;
          end;

          if ASGE_FSDK_GenderEstimation_Preview(
            FFaceGenderEngine, //[in] Gender estimation engine
            @offInput, //[in] the original imGender information
            //[in] the face rectangles information
            @lFaceRes_Gender,
            //[out] the results of Gender estimation
            lGenderRes
            ) = MOK then
            //分解人脸性别
            ExtractFaceGenders(lGenderRes, lGenders);

        end;

        for i := 0 to iFaces - 1 do
        begin
          lFaceInfo.Init;
          lFaceInfo.FaceRect := ArrFaceRect[i];
          lFaceInfo.FaceOrient := ArrFaceOrient[i];
          if i < Length(lAges) then
            lFaceInfo.Age := lAges[i];
          if i < Length(lGenders) then
            lFaceInfo.Gender := lGenders[i];
          AFaceInfos.Add(lFaceInfo);
        end;
      end;

    end;
  finally
    FreeAndNil(lFaceRegions);
  end;

  if pImgData <> nil then
    FreeMem(pImgData);

end;

{-------------------------------------------------------------------------------
 过程名:    TArcFaceSDK.UnInitialFaceTrackingEngine
 功能:      释放人脸跟踪引擎
 作者:      Bird
 日期:      2017.09.24
 参数:      无
 返回值:    Integer
 -------------------------------------------------------------------------------}
function TArcFaceSDK.UnInitialFaceTrackingEngine: Integer;
begin
  if FFaceTrackingEngine <> nil then
  begin
    Result := AFT_FSDK_UninitialFaceEngine(FFaceTrackingEngine);
    if Result = MOK then
      FFaceTrackingEngine := nil;
  end
  else
    Result := MOK;

  if FpFaceTrackingBuf <> nil then
  begin
    FreeMem(FpFaceTrackingBuf);
    FpFaceTrackingBuf := nil;
  end;

end;

{-------------------------------------------------------------------------------
 过程名:    TArcFaceSDK.UnInitialFaceRecognitionEngine
 功能:      释放人脸识别引擎
 作者:      Bird
 日期:      2017.09.24
 参数:      无
 返回值:    Integer
 -------------------------------------------------------------------------------}
function TArcFaceSDK.UnInitialFaceRecognitionEngine: Integer;
begin
  if FFaceRecognitionEngine <> nil then
  begin
    Result := AFR_FSDK_UninitialEngine(FFaceRecognitionEngine);
    if Result = MOK then
      FFaceRecognitionEngine := nil;
  end
  else
    Result := MOK;

  if FpFaceRecognitionBuf <> nil then
  begin
    FreeMem(FpFaceRecognitionBuf);
    FpFaceRecognitionBuf := nil;
  end;

end;

{-------------------------------------------------------------------------------
 过程名:    TArcFaceSDK.UnInitialFaceAgeEngine
 功能:      释放人脸识别引擎
 作者:      Bird
 日期:      2017.09.24
 参数:      无
 返回值:    Integer
 -------------------------------------------------------------------------------}
function TArcFaceSDK.UnInitialFaceAgeEngine: Integer;
begin
  if FFaceAgeEngine <> nil then
  begin
    Result := ASAE_FSDK_UninitAgeEngine(FFaceAgeEngine);
    if Result = MOK then
      FFaceAgeEngine := nil;
  end
  else
    Result := MOK;

  if FpFaceAgeBuf <> nil then
  begin
    FreeMem(FpFaceAgeBuf);
    FpFaceAgeBuf := nil;
  end;

end;

{-------------------------------------------------------------------------------
 过程名:    TArcFaceSDK.UnInitialFaceGenderEngine
 功能:      释放人脸识别引擎
 作者:      Bird
 日期:      2017.09.24
 参数:      无
 返回值:    Integer
 -------------------------------------------------------------------------------}
function TArcFaceSDK.UnInitialFaceGenderEngine: Integer;
begin
  if FFaceGenderEngine <> nil then
  begin
    Result := ASGE_FSDK_UninitGenderEngine(FFaceGenderEngine);
    if Result = MOK then
      FFaceGenderEngine := nil;
  end
  else
    Result := MOK;

  if FpFaceGenderBuf <> nil then
  begin
    FreeMem(FpFaceGenderBuf);
    FpFaceGenderBuf := nil;
  end;

end;

constructor TFaceModels.Create;
begin
  inherited;
  FModels := TList<AFR_FSDK_FACEMODEL>.Create;
end;

destructor TFaceModels.Destroy;
begin
  Clear;
  FModels.Free;
  inherited;
end;

function TFaceModels.AddModel(AModel: AFR_FSDK_FACEMODEL): Integer;
begin
  Result := FModels.Add(AModel);
end;

procedure TFaceModels.Assign(ASource: TFaceModels);
begin
  Clear;
  AddModels(ASource);
end;

procedure TFaceModels.AddModels(ASource: TFaceModels);
var
  i: Integer;
  lSourceModel, lDestModel: AFR_FSDK_FACEMODEL;
begin
  for i := 0 to ASource.Count - 1 do
  begin
    lSourceModel := ASource.FaceModel[i];

    lDestModel.lFeatureSize := lSourceModel.lFeatureSize;
    GetMem(lDestModel.pbFeature, lDestModel.lFeatureSize);
    CopyMemory(lDestModel.pbFeature, lSourceModel.pbFeature,
      lDestModel.lFeatureSize);

    FModels.Add(lDestModel);
  end;

end;

procedure TFaceModels.Clear;
var
  i: Integer;
begin
  if FModels.Count > 0 then
    for i := FModels.Count - 1 downto 0 do
    begin
      if FModels.Items[i].pbFeature <> nil then
      begin
        FreeMem(FModels.Items[i].pbFeature);
      end;
      FModels.Delete(i);
    end;
end;

procedure TFaceModels.Delete(Index: Integer);
begin
  if (Index >= 0) and (Index < FModels.Count) then
  begin
    if FModels.Items[Index].pbFeature <> nil then
      FreeMem(FModels.Items[Index].pbFeature);
    FModels.Delete(Index);
  end;
end;

function TFaceModels.GetCount: Integer;
begin
  Result := FModels.Count;
end;

function TFaceModels.GetFaceModel(Index: Integer): AFR_FSDK_FACEMODEL;
begin
  Result.lFeatureSize := 0;
  Result.pbFeature := nil;
  if (Index >= 0) and (Index < FModels.Count) then
    Result := FModels.Items[Index];
end;

function TFaceModels.GetItems(Index: Integer): AFR_FSDK_FACEMODEL;
begin
  Result.lFeatureSize := 0;
  Result.pbFeature := nil;
  if (Index >= 0) and (Index < FModels.Count) then
    Result := FModels.Items[Index];
end;

function TuJpegImage.BitmapData: TBitmap;
begin
  Result := Bitmap;
end;

constructor TEdzFaceModels.Create;
begin
  inherited;
  FRyID := '';
  FParams := '';
end;

procedure TEdzFaceModels.Assign(ASource: TFaceModels);
begin
  inherited;
  if ASource is TEdzFaceModels then
  begin
    with TEdzFaceModels(ASource) do
    begin
      Self.FRyID := RyID;
      Self.FParams := Params;
    end;
  end;
end;

procedure TEdzFaceModels.Clear;
var
  i: Integer;
begin
  inherited;
  FRyID := '';
  FParams := '';
end;

procedure TFaceBaseInfo.Init;
begin
  Age := 0;
  Gender := 0;
  FaceOrient := 0;
  with FaceRect do
  begin
    left := 0;
    right := 0;
    top := 0;
    bottom := 0;
  end;

end;

procedure TFaceFullInfo.Init;
begin
  Age := 0;
  Gender := 0;
  FaceOrient := 0;
  with FaceRect do
  begin
    left := 0;
    right := 0;
    top := 0;
    bottom := 0;
  end;

end;

end.
