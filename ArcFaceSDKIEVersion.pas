(*******************************************************
 * 虹软人脸识别SDK封装 ImageEN 版
 * 版权所有 (C) 2017 NJTZ  eMail:yhdgs@qq.com
 *******************************************************)

unit ArcFaceSDKIEVersion;

interface

uses Windows, Messages, SysUtils, System.Classes, amcomdef, ammemDef,
  arcsoft_fsdk_face_detection,
  arcsoft_fsdk_face_recognition,
  arcsoft_fsdk_face_tracking, asvloffscreendef, merrorDef,
  arcsoft_fsdk_age_estimation, arcsoft_fsdk_gender_estimation,
  Vcl.Graphics, Vcl.Imaging.jpeg, System.Generics.Collections, ArcFaceSDK,
  hyieutils, hyiedefs, imageenio, imageenproc, imageenview, System.Math;

type

  TResampleOptions = record
    NewWidth: Integer;
    NewHeight: Integer;
    Filter: TResampleFilter;
  end;

  TArcFaceSDKIEVersion = class(TArcFaceSDK)
  private
    FContrastRatio: Double;
    FResampleFilter: TResampleFilter;
    FResampleHeight: Integer;
    FResampleWidth: Integer;
    FImgDataInfoCache: TImgdataInfo;
  public
    constructor Create;
    destructor Destroy; override;
    class function CropFace(ASouceIEBitmap, ADestBitmap: TIEBitmap; AFaceRegion:
      AFR_FSDK_FACEINPUT; AExtendRatio, AResampleWidth, AResampleHeight: Integer;
      AResampleFilter: TResampleFilter): Boolean;
    function DetectAndRecognitionFacesFromIEBitmap(ABitmap: TIEBitmap; var
      AFaceRegions: TList<AFR_FSDK_FACEINPUT>; var AFaceModels: TFaceModels;
      AOutIEBitmp: TIEBitmap; AResampleWidth, AResampleHeight: Integer;
      AResampleFilter: TResampleFilter; AContrast: Double; AUseCache: Boolean):
      Boolean; overload;
    function DetectAndRecognitionFacesFromIEBitmap(ABitmap: TIEBitmap; var
      AFaceRegions: TList<AFR_FSDK_FACEINPUT>; var AFaceModels: TFaceModels;
      AOutIEBitmp: TIEBitmap; AUseCache: Boolean): Boolean; overload;
    function DetectAndRecognitionFacesFromBitmapEX(ABitmap: TBitmap; var
      AFaceRegions: TList<AFR_FSDK_FACEINPUT>; var AFaceModels: TFaceModels;
      AOutIEBitmp: TIEBitmap; AResampleWidth, AResampleHeight: Integer;
      AResampleFilter: TResampleFilter; AContrast: Double; AUseCache: Boolean):
      Boolean; overload;
    function DetectFacesAndAgeGenderFromBitmapEX(ABitmap: TBitmap;
      var AFaceInfos:
      TList<TFaceBaseInfo>; AOutIEBitmp: TIEBitmap; AResampleWidth,
      AResampleHeight: Integer; AResampleFilter: TResampleFilter; AContrast:
      Double; AUseCache: Boolean): Boolean; overload;
    function DetectAndRecognitionFacesFromFileEx(AFileName: string; var
      AFaceRegions: TList<AFR_FSDK_FACEINPUT>; var AFaceModels: TFaceModels;
      AOutIEBitmp: TIEBitmap; AResampleWidth, AResampleHeight: Integer;
      AResampleFilter: TResampleFilter; AContrast: Double; AUseCache: Boolean):
      Boolean; overload;
    function DRAGfromIEBitmap(ABitmap: TIEBitmap; var AFaceInfos:
      TList<TFaceBaseInfo>; var AFaceModels: TFaceModels; AOutIEBitmp: TIEBitmap;
      AResampleWidth, AResampleHeight: Integer; AResampleFilter: TResampleFilter;
      AContrast: Double; AUseCache: Boolean): Boolean; overload;
    function DetectFacesAndAgeGenderFromIEBitmap(ABitmap: TIEBitmap; var
      AFaceInfos: TList<TFaceBaseInfo>; AOutIEBitmp: TIEBitmap; AUseCache:
      Boolean): Boolean; overload;
    function DetectFacesAndAgeGenderFromFileEx(AFileName: string; var AFaceInfos:
      TList<TFaceBaseInfo>; AOutIEBitmp: TIEBitmap; AResampleWidth,
      AResampleHeight: Integer; AResampleFilter: TResampleFilter; AContrast:
      Double; AUseCache: Boolean): Boolean; overload;
    function DetectFacesFromIEBitmap(ABitmap: TIEBitmap; var AFaceRegions:
      TList<AFR_FSDK_FACEINPUT>; AOutIEBitmp: TIEBitmap; AResampleWidth,
      AResampleHeight: Integer; AResampleFilter: TResampleFilter; AContrast:
      Double; AUseCache: Boolean): Boolean; overload;
    function DetectFacesFromFile(AFile: string; var AFaceRegions:
      TList<AFR_FSDK_FACEINPUT>; AOutIEBitmp: TIEBitmap; AResampleWidth,
      AResampleHeight: Integer; AResampleFilter: TResampleFilter; AContrast:
      Double; AUseCache: Boolean): Boolean; overload;
    function DetectFacesFromFile(AFile: string; var AFaceRegions:
      TList<AFR_FSDK_FACEINPUT>; AOutIEBitmp: TIEBitmap; AUseCache: Boolean):
      Boolean; overload;
    function DetectFacesFromIEBitmap(ABitmap: TIEBitmap; var AFaceRegions:
      TList<AFR_FSDK_FACEINPUT>; AOutIEBitmp: TIEBitmap; AUseCache: Boolean):
      Boolean; overload;
    class procedure DrawFaceRectAgeGenderEX(AView: TImageEnView;
      AFaceIdx: Integer;
      AFaceInfo: TFaceBaseInfo; AColor: TColor = clBlue; AWidth: Integer = 2;
      ADrawIndex: Boolean = true; ATextSize: Integer = 0);
    class procedure DrawFaceRectEX(AView: TImageEnView; AFaceIdx: Integer;
      AFaceInfo: AFR_FSDK_FACEINPUT; AColor: TColor = clBlue; AWidth: Integer =
      2; ADrawIndex: Boolean = true; ATextSize: Integer = 0);
    function ExtractFaceFeatureFromIEBitmap(AIEBitmap: TIEBitmap; AFaceRegion:
      AFR_FSDK_FACEINPUT; var AFaceModel: AFR_FSDK_FACEMODEL; AOutIEBitmp:
      TIEBitmap; AResampleWidth, AResampleHeight: Integer; AResampleFilter:
      TResampleFilter; AUseCache: Boolean): Boolean;
    function MatchFaceWithIEBitmaps(AInBitmap1, AInBitmap2, AOutIEBitmp1,
      AOutIEBitmp2: TIEBitmap; AResampleWidth, AResampleHeight: Integer;
      AResampleFilter: TResampleFilter; AContrast: Double; AUseCache: Boolean):
      Single; overload;
    function MatchFaceWithIEBitmaps(AInBitmap1, AInBitmap2, AOutIEBitmp1,
      AOutIEBitmp2: TIEBitmap; AContrast: Double; AUseCache: Boolean): Single;
      overload;
    function TrackFacesFromIEBitmap(ABitmap: TIEBitmap; var AFaceRegions:
      TList<AFR_FSDK_FACEINPUT>; AUseCache: Boolean): Boolean;
    class function ReadFromFile(AFileName: string; var AImgData: TImgdataInfo;
      AOutIEBitmp: TIEBitmap; AResampleWidth: Integer = 0; AResampleHeight:
      Integer = 0; AResampleFilter: TResampleFilter = rfNone; AContrast: Double =
      0): Boolean;
    class function ReadIEBitmap(ABitmap: TIEBitmap;
      var AImgDataInfo: TImgdataInfo;
      AOutIEBitmp: TIEBitmap; AResampleWidth: Integer = 0; AResampleHeight:
      Integer = 0; AResampleFilter: TResampleFilter = rfNone; AContrast: Double =
      0): Boolean; overload;
    function TrackFacesAndAgeGenderFromIEBitmap(ABitmap: TIEBitmap;
      var AFaceInfos:
      TList<TFaceBaseInfo>; AUseCache: Boolean): Boolean;
    function DetectFacesAndAgeGenderFromIEBitmap(ABitmap: TIEBitmap; var
      AFaceInfos: TList<TFaceBaseInfo>; AOutIEBitmp: TIEBitmap; AResampleWidth,
      AResampleHeight: Integer; AResampleFilter: TResampleFilter; AContrast:
      Double; AUseCache: Boolean): Boolean; overload;
    function DRAGfromIEBitmap(ABitmap: TIEBitmap; var AFaceInfos:
      TList<TFaceBaseInfo>; var AFaceModels: TFaceModels; AOutIEBitmp: TIEBitmap;
      AUseCache: Boolean): Boolean; overload;
    function DRAGfromFile(AFileName: string;
      var AFaceInfos: TList<TFaceBaseInfo>;
      var AFaceModels: TFaceModels; AOutIEBitmp: TIEBitmap; AUseCache: Boolean):
      Boolean; overload;
    function DRAGfromFile(AFileName: string;
      var AFaceInfos: TList<TFaceBaseInfo>;
      var AFaceModels: TFaceModels; AOutIEBitmp: TIEBitmap; AResampleWidth,
      AResampleHeight: Integer; AResampleFilter: TResampleFilter; AContrast:
      Double; AUseCache: Boolean): Boolean; overload;
  published
    property ContrastRatio: Double read FContrastRatio write FContrastRatio;
    property ResampleFilter: TResampleFilter read FResampleFilter write
      FResampleFilter;
    property ResampleHeight: Integer read FResampleHeight write FResampleHeight;
    property ResampleWidth: Integer read FResampleWidth write FResampleWidth;
  end;

implementation

constructor TArcFaceSDKIEVersion.Create;
begin
  inherited;
  FResampleFilter := rfLanczos3;
  FResampleWidth := 0;
  FResampleHeight := 0;
  FContrastRatio := 0;
  FImgDataInfoCache.pImgData := nil;
end;

destructor TArcFaceSDKIEVersion.Destroy;
begin
  if FImgDataInfoCache.pImgData <> nil then
    FreeMem(FImgDataInfoCache.pImgData);
  inherited;
end;

//根据人脸框信息裁剪人脸到新文件
class function TArcFaceSDKIEVersion.CropFace(
  ASouceIEBitmap, //源BMP
  ADestBitmap: TIEBitmap; //目标BMP
  AFaceRegion: AFR_FSDK_FACEINPUT; //人脸区域
  AExtendRatio, //扩展比率，最小为0，实际应用再除以100
  AResampleWidth, //目标图像宽度,0表示不变或随高度变动
  AResampleHeight: Integer; //目标图像高度,0表示不变或随宽度变动
  AResampleFilter: TResampleFilter//缩放滤镜
  ): Boolean;
var
  iLeft, iTop, iRight, iBottom, iWidthExtend, iHeightExtend, iTmp: Integer;
  fHvsW: Double;
  iWidth, iHeight: Integer;
begin
  Result := False;
  if ASouceIEBitmap = nil then
    Exit;

  //图像高宽比，如果缩放高宽其中有一为0表示维持比率不变
  if (AResampleWidth <= 0) or (AResampleHeight <= 0) then
    fHvsW := 0
  else
    fHvsW := AResampleHeight / AResampleWidth;

  //图像从人脸框向四周扩散的像素
  iWidthExtend := Round((AFaceRegion.rcFace.right - AFaceRegion.rcFace.left) *
    (AExtendRatio / 100));
  iHeightExtend :=
    Round((AFaceRegion.rcFace.bottom - AFaceRegion.rcFace.top) *
    (AExtendRatio / 100));

  //向四周扩展
  iLeft := AFaceRegion.rcFace.left - iWidthExtend;
  iRight := AFaceRegion.rcFace.right + iWidthExtend;
  iTop := AFaceRegion.rcFace.top - iHeightExtend;
  iBottom := AFaceRegion.rcFace.bottom + iHeightExtend;

  //临时宽高
  iWidth := Abs(iRight - iLeft);
  iHeight := Abs(iBottom - iTop);

  //如果 高/宽 > 1，则宽度保持不变，再次扩展高度
  if fHvsW > 1 then
  begin
    iTmp := (Max(Round(iWidth * fHvsW), iHeight) - iHeight) div 2;
    //重新定为顶、底
    iTop := iTop - iTmp;
    iBottom := iBottom + iTmp;
  end
  //如果 0 < 高/宽 <= 1，则高度保持不变，再次扩展宽度
  else if fHvsW > 0 then
  begin
    iTmp := (Max(Round(iHeight / fHvsW), iWidth) - iWidth) div 2;
    iLeft := iLeft - iTmp;
    iRight := iRight + iTmp;
  end;

  //如果顶小于0并且底大于原始图像高度
  if (iTop < 0) and (iBottom > ASouceIEBitmap.Height) then
  begin
    //顶置0
    iTop := 0;
    //底置原始图像高度
    iBottom := ASouceIEBitmap.Height;
  end
  //仅顶小于0
  else if iTop < 0 then
  begin
    //扩展底
    iBottom := Min(iBottom - iTop, ASouceIEBitmap.Height);
    iTop := 0;
  end
  //仅底大于原始图像高度
  else if iBottom > ASouceIEBitmap.Height then
  begin
    //扩展顶
    iTop := Max(iTop - (iBottom - ASouceIEBitmap.Height), 0);
    iBottom := ASouceIEBitmap.Height;
  end;

  //如果左边小于0并且右边大于原始图像宽度
  if (iLeft < 0) and (iRight > ASouceIEBitmap.Width) then
  begin
    iLeft := 0;
    iRight := ASouceIEBitmap.Width;
  end
  //仅左边小于0
  else if iLeft < 0 then
  begin
    iRight := Min(iRight - iLeft, ASouceIEBitmap.Width);
    iLeft := 0;
  end
  //仅右边大于原始图像宽度
  else if iRight > ASouceIEBitmap.Width then
  begin
    iLeft := Max(iLeft - (iRight - ASouceIEBitmap.Width), 0);
    iRight := ASouceIEBitmap.Width;
  end;

  if ADestBitmap = nil then
    ADestBitmap := TIEBitmap.Create;

  //设置目标图像宽高
  ADestBitmap.Width := Abs(iRight - iLeft);
  ADestBitmap.Height := Abs(iBottom - iTop);

  //拷贝图像
  ASouceIEBitmap.CopyRectTo(ADestBitmap, iLeft, iTop, 0, 0, Abs(iRight - iLeft),
    Abs(iBottom - iTop));
end;

//从IEBitmap中获取人脸位置和特征信息列表
function TArcFaceSDKIEVersion.DetectAndRecognitionFacesFromIEBitmap(
  ABitmap: TIEBitmap; //源位图
  var AFaceRegions: TList<AFR_FSDK_FACEINPUT>; //人脸框信息列表
  var AFaceModels: TFaceModels; //人脸特征集
  AOutIEBitmp: TIEBitmap; //缩放后输出的目标位图
  AResampleWidth, //缩放宽度
  AResampleHeight: Integer; //绽放高度
  AResampleFilter: TResampleFilter; //缩放滤镜
  AContrast: Double; //对比度，默认0不调整
  AUseCache: Boolean//是否使用缓存空间，可以避免频繁申请释放内存，应注意线程安全
  ): Boolean;
var
  lImgData: TImgdataInfo;
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

  //使用缓存
  if AUseCache then
    lImgData := FImgDataInfoCache
  else
    lImgData.pImgData := nil;

{$IFDEF DEBUG}
  T := GetTickCount;
{$ENDIF}
  if not ReadIEBitmap(ABitmap, lImgData, AOutIEBitmp, AResampleWidth,
    AResampleHeight, AResampleFilter, AContrast) then
    Exit;
{$IFDEF DEBUG}
  T := GetTickCount - T;
  DoLog('载入数据耗时：' + IntToStr(T));
{$ENDIF}
  offInput.u32PixelArrayFormat := ASVL_PAF_RGB24_B8G8R8;
  FillChar(offInput.pi32Pitch, SizeOf(offInput.pi32Pitch), 0);
  FillChar(offInput.ppu8Plane, SizeOf(offInput.ppu8Plane), 0);

  offInput.i32Width := lImgData.Width;
  offInput.i32Height := lImgData.Height;

  offInput.ppu8Plane[0] := IntPtr(lImgData.pImgData);
  offInput.pi32Pitch[0] := lImgData.LineBytes;
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

  if not AUseCache then
  begin
    if lImgData.pImgData <> nil then
      FreeMem(lImgData.pImgData)
  end
  else
  begin
    FImgDataInfoCache := lImgData;
  end;

end;

//从IEBitmap中获取人脸位置和特征信息列表，缩放参数使用本对象实例的缩放参数属性
//ResampleWidth、ResampleHeight、ResampleFilter等参数使用类实例相应属性
function TArcFaceSDKIEVersion.DetectAndRecognitionFacesFromIEBitmap(
  ABitmap: TIEBitmap; //源位图
  var AFaceRegions: TList<AFR_FSDK_FACEINPUT>; //人脸框信息列表
  var AFaceModels: TFaceModels; //人脸特征集
  AOutIEBitmp: TIEBitmap; //缩放后输出的目标位图
  AUseCache: Boolean//是否使用缓存空间，可以避免频繁申请释放内存，应注意线程安全
  ): Boolean;
begin
  Result := DetectAndRecognitionFacesFromIEBitmap(ABitmap, AFaceRegions,
    AFaceModels, AOutIEBitmp, FResampleWidth, FResampleHeight, FResampleFilter,
    FContrastRatio, AUseCache);
end;

//从标准Bitmap获取人脸位置和特征信息列表
function TArcFaceSDKIEVersion.DetectAndRecognitionFacesFromBitmapEX(
  ABitmap: TBitmap; //源位图
  var AFaceRegions: TList<AFR_FSDK_FACEINPUT>; //人脸框信息列表
  var AFaceModels: TFaceModels; //人脸特征集
  AOutIEBitmp: TIEBitmap; //缩放后输出的目标位图
  AResampleWidth, //缩放宽度
  AResampleHeight: Integer; //缩放高度
  AResampleFilter: TResampleFilter; //缩放滤镜
  AContrast: Double; //对比度，默认0不调整
  AUseCache: Boolean//是否使用缓存空间，使用可以避免频繁申请释放内存，应注意线程安全
  ): Boolean;
var
  lIEBitmap: TIEBitmap;
begin
  lIEBitmap := TIEBitmap.Create;
  try
    lIEBitmap.Assign(ABitmap);
    Result := DetectAndRecognitionFacesFromIEBitmap(lIEBitmap, AFaceRegions,
      AFaceModels, AOutIEBitmp, AResampleWidth, AResampleHeight,
      AResampleFilter, AContrast, AUseCache);
  finally
    lIEBitmap.Free;
  end;
end;

//从标准位图中获取人脸位置、性别和年龄信息列表
function TArcFaceSDKIEVersion.DetectFacesAndAgeGenderFromBitmapEX(
  ABitmap: TBitmap; //源位图
  var AFaceInfos: TList<TFaceBaseInfo>; //人脸基本信息列表
  AOutIEBitmp: TIEBitmap; //缩放后输出的目标位图
  AResampleWidth, //缩放宽度
  AResampleHeight: Integer; //缩放高度
  AResampleFilter: TResampleFilter; //缩放滤镜
  AContrast: Double; //对比度，默认0不调整
  AUseCache: Boolean//是否使用缓存空间，使用可以避免频繁申请释放内存，应注意线程安全
  ): Boolean;
var
  lIEBitmap: TIEBitmap;
begin
  lIEBitmap := TIEBitmap.Create;
  try
    lIEBitmap.Assign(ABitmap);
    Result := DetectFacesAndAgeGenderFromIEBitmap(lIEBitmap, AFaceInfos,
      AOutIEBitmp, AResampleWidth, AResampleHeight, AResampleFilter, AContrast,
      AUseCache);
  finally
    lIEBitmap.Free;
  end;
end;

//从图像文件中检测人脸位置信息列表并提取所有检测到的人脸特征信息
//支持常见格式图像文件
function TArcFaceSDKIEVersion.DetectAndRecognitionFacesFromFileEx(
  AFileName: string; //文件名
  var AFaceRegions: TList<AFR_FSDK_FACEINPUT>; //人脸框信息列表
  var AFaceModels: TFaceModels; //人脸特征集
  AOutIEBitmp: TIEBitmap; //缩放后输出的目标位图
  AResampleWidth, //缩放宽度
  AResampleHeight: Integer; //缩放高度
  AResampleFilter: TResampleFilter; //缩放滤镜
  AContrast: Double; //对比度，默认0不调整
  AUseCache: Boolean//是否使用缓存空间，使用可以避免频繁申请释放内存，应注意线程安全
  ): Boolean;
var
  lIEBitmap: TIEBitmap;
  io: TImageEnIO;
begin
  Result := False;
  if not FileExists(AFileName) then
    Exit;

  lIEBitmap := TIEBitmap.Create;
  io := TImageEnIO.Create(nil);
  try
    io.AttachedIEBitmap := lIEBitmap;
    io.LoadFromFile(AFileName);
    Result := DetectAndRecognitionFacesFromIEBitmap(lIEBitmap, AFaceRegions,
      AFaceModels, AOutIEBitmp, AResampleWidth, AResampleHeight,
      AResampleFilter, AContrast, AUseCache);
  finally
    lIEBitmap.Free;
    io.Free;
  end;
end;

//从IEBitmap中获取人脸位置、年龄、性别和特征信息列表
function TArcFaceSDKIEVersion.DRAGfromIEBitmap(
  ABitmap: TIEBitmap; //源位图
  var AFaceInfos: TList<TFaceBaseInfo>; //人脸基本信息列表
  var AFaceModels: TFaceModels; //人脸特征集
  AOutIEBitmp: TIEBitmap; //缩放后输出的目标位图
  AResampleWidth, //缩放宽度
  AResampleHeight: Integer; //缩放高度
  AResampleFilter: TResampleFilter; //缩放滤镜
  AContrast: Double; //对比度，默认0不调整
  AUseCache: Boolean//是否使用缓存空间，使用可以避免频繁申请释放内存，应注意线程安全
  ): Boolean;
var
  lImgData: TImgdataInfo;
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

  //使用缓存
  if AUseCache then
    lImgData := FImgDataInfoCache
  else
    lImgData.pImgData := nil;

{$IFDEF DEBUG}
  T := GetTickCount;
{$ENDIF}
  if not ReadIEBitmap(ABitmap, lImgData, AOutIEBitmp, AResampleWidth,
    AResampleHeight, AResampleFilter, AContrast) then
    Exit;
{$IFDEF DEBUG}
  T := GetTickCount - T;
  DoLog('载入数据耗时：' + IntToStr(T));
{$ENDIF}
  offInput.u32PixelArrayFormat := ASVL_PAF_RGB24_B8G8R8;
  FillChar(offInput.pi32Pitch, SizeOf(offInput.pi32Pitch), 0);
  FillChar(offInput.ppu8Plane, SizeOf(offInput.ppu8Plane), 0);

  offInput.i32Width := lImgData.Width;
  offInput.i32Height := lImgData.Height;

  offInput.ppu8Plane[0] := IntPtr(lImgData.pImgData);
  offInput.pi32Pitch[0] := lImgData.LineBytes;
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

  if not AUseCache then
  begin
    if lImgData.pImgData <> nil then
      FreeMem(lImgData.pImgData)
  end
  else
  begin
    FImgDataInfoCache := lImgData;
  end;

end;

//从TIEBitmap中获取人脸位置、性别和年龄信息列表,部分参数使用类实例属性
function TArcFaceSDKIEVersion.DetectFacesAndAgeGenderFromIEBitmap(
  ABitmap: TIEBitmap; //源位图
  var AFaceInfos: TList<TFaceBaseInfo>; //人脸基本信息列表
  AOutIEBitmp: TIEBitmap; //缩放后输出的目标位图
  AUseCache: Boolean//是否使用缓存空间，使用可以避免频繁申请释放内存，应注意线程安全
  ): Boolean;
begin
  Result := DetectFacesAndAgeGenderFromIEBitmap(ABitmap, AFaceInfos,
    AOutIEBitmp, FResampleWidth, FResampleHeight, FResampleFilter,
    FContrastRatio, AUseCache);
end;

//从图像文件中获取人脸位置、性别和年龄信息列表
function TArcFaceSDKIEVersion.DetectFacesAndAgeGenderFromFileEx(
  AFileName: string; //图像文件名
  var AFaceInfos: TList<TFaceBaseInfo>; //人脸基本信息列表
  AOutIEBitmp: TIEBitmap; //缩放后输出的目标位图
  AResampleWidth, //缩放宽度
  AResampleHeight: Integer; //缩放高度
  AResampleFilter: TResampleFilter; //缩放滤镜
  AContrast: Double; //对比度，默认0不调整
  AUseCache: Boolean//是否使用缓存空间，使用可以避免频繁申请释放内存，应注意线程安全
  ): Boolean;
var
  lIEBitmap: TIEBitmap;
  io: TImageEnIO;
begin
  Result := False;
  if not FileExists(AFileName) then
    Exit;

  lIEBitmap := TIEBitmap.Create;
  io := TImageEnIO.Create(nil);
  try
    io.AttachedIEBitmap := lIEBitmap;
    io.LoadFromFile(AFileName);
    Result := DetectFacesAndAgeGenderFromIEBitmap(lIEBitmap, AFaceInfos,
      AOutIEBitmp, AResampleWidth, AResampleHeight, AResampleFilter, AContrast,
      AUseCache);
  finally
    lIEBitmap.Free;
    io.Free;
  end;
end;

//从IEBitmam中获取人脸位置信息列表
function TArcFaceSDKIEVersion.DetectFacesFromIEBitmap(
  ABitmap: TIEBitmap; //源位图
  var AFaceRegions: TList<AFR_FSDK_FACEINPUT>; //人脸框信息列表
  AOutIEBitmp: TIEBitmap; //缩放后输出的目标位图
  AResampleWidth, //缩放宽度
  AResampleHeight: Integer; //缩放高度
  AResampleFilter: TResampleFilter; //缩放滤镜
  AContrast: Double; //对比度，默认0不调整
  AUseCache: Boolean//是否使用缓存空间，使用可以避免频繁申请释放内存，应注意线程安全
  ): Boolean;
var
  offInput: ASVLOFFSCREEN;
  pFaceRes: LPAFD_FSDK_FACERES;
  lImgData: TImgdataInfo;
begin
  Result := False;

  if FFaceDetectionEngine = nil then
    Exit;

  if AUseCache then
    lImgData := FImgDataInfoCache
  else
    lImgData.pImgData := nil;

  if not ReadIEBitmap(ABitmap, lImgData, AOutIEBitmp, AResampleWidth,
    AResampleHeight, AResampleFilter, AContrast) then
    Exit;

  offInput.u32PixelArrayFormat := ASVL_PAF_RGB24_B8G8R8;
  FillChar(offInput.pi32Pitch, SizeOf(offInput.pi32Pitch), 0);
  FillChar(offInput.ppu8Plane, SizeOf(offInput.ppu8Plane), 0);

  offInput.i32Width := lImgData.Width;
  offInput.i32Height := lImgData.Height;

  offInput.ppu8Plane[0] := IntPtr(lImgData.pImgData);
  offInput.pi32Pitch[0] := lImgData.LineBytes;
  //人脸检测
  if AFD_FSDK_StillImageFaceDetection(FFaceDetectionEngine, @offInput, pFaceRes)
    = MOK then
  begin
    ExtractFaceBoxs(pFaceRes^, AFaceRegions);
  end;

  if not AUseCache then
  begin
    if lImgData.pImgData <> nil then
      FreeMem(lImgData.pImgData)
  end
  else
  begin
    FImgDataInfoCache := lImgData;
  end;

end;

//从图像文件中获取人脸位置信息列表
function TArcFaceSDKIEVersion.DetectFacesFromFile(
  AFile: string; //图像文件名
  var AFaceRegions: TList<AFR_FSDK_FACEINPUT>; //人脸框信息列表
  AOutIEBitmp: TIEBitmap; //缩放后输出的目标位图
  AResampleWidth, //缩放宽度
  AResampleHeight: Integer; //缩放高度
  AResampleFilter: TResampleFilter; //缩放滤镜
  AContrast: Double; //对比度，默认0不调整
  AUseCache: Boolean//是否使用缓存空间，使用可以避免频繁申请释放内存，应注意线程安全
  ): Boolean;
var
  lImgData: TImgdataInfo;
  offInput: ASVLOFFSCREEN;
  pFaceRes: LPAFD_FSDK_FACERES;
begin
  Result := False;

  if FFaceDetectionEngine = nil then
    Exit;

  if AUseCache then
    lImgData := FImgDataInfoCache
  else
    lImgData.pImgData := nil;

  if not ReadFromFile(AFile, lImgData, AOutIEBitmp,
    AResampleWidth, AResampleHeight, AResampleFilter, AContrast) then
    Exit;

  offInput.u32PixelArrayFormat := ASVL_PAF_RGB24_B8G8R8;
  FillChar(offInput.pi32Pitch, SizeOf(offInput.pi32Pitch), 0);
  FillChar(offInput.ppu8Plane, SizeOf(offInput.ppu8Plane), 0);

  offInput.i32Width := lImgData.Width;
  offInput.i32Height := lImgData.Height;

  offInput.ppu8Plane[0] := IntPtr(lImgData.pImgData);
  offInput.pi32Pitch[0] := lImgData.LineBytes;

  //人脸检测
  if AFD_FSDK_StillImageFaceDetection(FFaceDetectionEngine, @offInput, pFaceRes)
    = MOK then
  begin
    ExtractFaceBoxs(pFaceRes^, AFaceRegions);
  end;

  if not AUseCache then
  begin
    if lImgData.pImgData <> nil then
      FreeMem(lImgData.pImgData)
  end
  else
  begin
    FImgDataInfoCache := lImgData;
  end;

end;

//从文件中获取人脸位置信息列表，部分参数使用类实例属性值
function TArcFaceSDKIEVersion.DetectFacesFromFile(
  AFile: string; //图像文件
  var AFaceRegions: TList<AFR_FSDK_FACEINPUT>; //人脸框信息列表
  AOutIEBitmp: TIEBitmap; //缩放后输出目标位图
  AUseCache: Boolean//是否使用缓存空间
  ): Boolean;
begin
  Result := DetectFacesFromFile(AFile, AFaceRegions, AOutIEBitmp,
    FResampleWidth, FResampleHeight, FResampleFilter, FContrastRatio,
    AUseCache);
end;

//从IEBitmap中获取人脸位置信息列表，部分参数使用类实例属性值
function TArcFaceSDKIEVersion.DetectFacesFromIEBitmap(
  ABitmap: TIEBitmap; //源位图
  var AFaceRegions: TList<AFR_FSDK_FACEINPUT>; //人脸框信息列表
  AOutIEBitmp: TIEBitmap; //缩放后输出的目标位图
  AUseCache: Boolean//是否使用缓存空间，使用可以避免频繁申请释放内存，应注意线程安全
  ): Boolean;
begin
  Result := DetectFacesFromIEBitmap(ABitmap, AFaceRegions, AOutIEBitmp,
    FResampleWidth, FResampleHeight, FResampleFilter, FContrastRatio,
    AUseCache);
end;

//从IEBitmap中获取人脸位置信息列表（追踪模式），部分参数使用类实例属性值
function TArcFaceSDKIEVersion.TrackFacesFromIEBitmap(
  ABitmap: TIEBitmap; //源位图
  var AFaceRegions: TList<AFR_FSDK_FACEINPUT>; //人脸框信息列表
  AUseCache: Boolean//是否使用缓存空间，使用可以避免频繁申请释放内存，应注意线程安全
  ): Boolean;
var
  lImgDataInfo: TImgdataInfo;
  offInput: ASVLOFFSCREEN;
  pFaceRes: LPAFT_FSDK_FACERES;
begin
  Result := False;

  if FFaceDetectionEngine = nil then
    Exit;

  if AUseCache then
    lImgDataInfo := FImgDataInfoCache
  else
    lImgDataInfo.pImgData := nil;

  if not ReadIEBitmap(ABitmap, lImgDataInfo, nil) then
    Exit;

  offInput.u32PixelArrayFormat := ASVL_PAF_RGB24_B8G8R8;
  FillChar(offInput.pi32Pitch, SizeOf(offInput.pi32Pitch), 0);
  FillChar(offInput.ppu8Plane, SizeOf(offInput.ppu8Plane), 0);

  offInput.i32Width := lImgDataInfo.Width;
  offInput.i32Height := lImgDataInfo.Height;

  offInput.ppu8Plane[0] := IntPtr(lImgDataInfo.pImgData);
  offInput.pi32Pitch[0] := lImgDataInfo.LineBytes;

  //人脸检测
  if AFT_FSDK_FaceFeatureDetect(FFaceTrackingEngine, @offInput, pFaceRes)
    = MOK then
  begin
    ExtractFaceBoxs(pFaceRes^, AFaceRegions);
  end;

  if not AUseCache then
  begin
    if lImgDataInfo.pImgData <> nil then
      FreeMem(lImgDataInfo.pImgData)
  end
  else
  begin
    FImgDataInfoCache := lImgDataInfo;
  end;
end;

//在ImageEnView上画人脸框、性别、年龄
class procedure TArcFaceSDKIEVersion.DrawFaceRectAgeGenderEX(
  AView: TImageEnView; //图像预览组件
  AFaceIdx: Integer; //人脸索引
  AFaceInfo: TFaceBaseInfo; //人脸基本信息
  AColor: TColor = clBlue; //画笔颜色
  AWidth: Integer = 2; //框线宽度
  ADrawIndex: Boolean = true; //是否画索引
  ATextSize: Integer = 0//字符大小
  );
begin
  DrawFaceRectAgeGender(AView.IEBitmap.Canvas, AFaceIdx, AFaceInfo, AColor,
    AWidth, ADrawIndex, ATextSize);
  AView.Update;
end;

//在ImageEnView上画人脸框
class procedure TArcFaceSDKIEVersion.DrawFaceRectEX(
  AView: TImageEnView; //图像预览组件
  AFaceIdx: Integer; //人脸索引
  AFaceInfo: AFR_FSDK_FACEINPUT; //人脸框信息
  AColor: TColor = clBlue; //画笔颜色
  AWidth: Integer = 2; //框线宽度
  ADrawIndex: Boolean = true; //是否画索引
  ATextSize: Integer = 0//文字大小
  );
begin
  DrawFaceRect(AView.IEBitmap.Canvas, AFaceIdx, AFaceInfo, AColor, AWidth,
    ADrawIndex, ATextSize);
  AView.Update;
end;

//从IEBitmap中提取单个人脸特征
function TArcFaceSDKIEVersion.ExtractFaceFeatureFromIEBitmap(
  AIEBitmap: TIEBitmap; //源位图
  AFaceRegion: AFR_FSDK_FACEINPUT; //人脸框信息
  var AFaceModel: AFR_FSDK_FACEMODEL; //人脸特征，特征数据内存需手动使用freemem释放
  AOutIEBitmp: TIEBitmap; //缩放后输出的目标位图
  AResampleWidth, //缩放宽度
  AResampleHeight: Integer; //缩放高度
  AResampleFilter: TResampleFilter; //缩放滤镜
  AUseCache: Boolean//是否使用缓存空间，使用可以避免频繁申请释放内存，应注意线程安全
  ): Boolean;
var
  lImgData: TImgdataInfo;
  offInput: ASVLOFFSCREEN;
  pFaceRes: LPAFD_FSDK_FACERES;
begin
  Result := False;

  if FFaceRecognitionEngine = nil then
    Exit;

  if AUseCache then
    lImgData := FImgDataInfoCache
  else
    lImgData.pImgData := nil;

  if not ReadIEBitmap(AIEBitmap, lImgData, AOutIEBitmp, AResampleWidth,
    AResampleHeight, AResampleFilter) then
    Exit;

  offInput.u32PixelArrayFormat := ASVL_PAF_RGB24_B8G8R8;
  FillChar(offInput.pi32Pitch, SizeOf(offInput.pi32Pitch), 0);
  FillChar(offInput.ppu8Plane, SizeOf(offInput.ppu8Plane), 0);
  offInput.i32Width := lImgData.Width;
  offInput.i32Height := lImgData.Height;

  offInput.ppu8Plane[0] := IntPtr(lImgData.pImgData);
  offInput.pi32Pitch[0] := lImgData.LineBytes;

  //人脸特征提取
  Result := ExtractFaceFeature(offInput, AFaceRegion, AFaceModel);

  if not AUseCache then
  begin
    if lImgData.pImgData <> nil then
      FreeMem(lImgData.pImgData)
  end
  else
  begin
    FImgDataInfoCache := lImgData;
  end;

end;

//比对两张人脸IEBitmap（只比对第一个人脸）
function TArcFaceSDKIEVersion.MatchFaceWithIEBitmaps(
  AInBitmap1, //待比较源图一
  AInBitmap2, //待比较源图二
  AOutIEBitmp1, //源图一缩放后输出的目标位图
  AOutIEBitmp2: TIEBitmap; //源图二绽放后输出的目标位图
  AResampleWidth, //缩放宽度
  AResampleHeight: Integer; //缩放高度
  AResampleFilter: TResampleFilter; //缩放滤镜
  AContrast: Double; //对比度，默认0不调整
  AUseCache: Boolean//是否使用缓存空间，使用可以避免频繁申请释放内存，应注意线程安全
  ): Single;
var
  AFaceRegions1, AFaceRegions2: TList<AFR_FSDK_FACEINPUT>;
  AFaceModels1, AFaceModels2: TFaceModels;
  i: Integer;
  T: Cardinal;
begin
  Result := 0;

  if (AInBitmap1 = nil) or (AInBitmap2 = nil) then
    Exit;

  AFaceRegions1 := TList<AFR_FSDK_FACEINPUT>.Create;
  AFaceRegions2 := TList<AFR_FSDK_FACEINPUT>.Create;
  AFaceModels1 := TFaceModels.Create;
  AFaceModels2 := TFaceModels.Create;
  try
{$IFDEF DEBUG}
    T := GetTickCount;
{$ENDIF}
    DetectAndRecognitionFacesFromIEBitmap(AInBitmap1, AFaceRegions1,
      AFaceModels1, AOutIEBitmp1, AResampleWidth, AResampleHeight,
      AResampleFilter, AContrast, AUseCache);
{$IFDEF DEBUG}
    T := GetTickCount - T;
    DoLog('取图一特征耗时：' + IntToStr(T));
{$ENDIF}

{$IFDEF DEBUG}
    T := GetTickCount;
{$ENDIF}
    DetectAndRecognitionFacesFromIEBitmap(AInBitmap2, AFaceRegions2,
      AFaceModels2, AOutIEBitmp2, AResampleWidth, AResampleHeight,
      AResampleFilter, AContrast, AUseCache);
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

//比对两张人脸IEBitmap（只比对第一个人脸），部分参数使用类实例的属性值
function TArcFaceSDKIEVersion.MatchFaceWithIEBitmaps(
  AInBitmap1, //待比较源图一
  AInBitmap2, //待比较源图二
  AOutIEBitmp1, //源图一缩放后输出的目标位图
  AOutIEBitmp2: TIEBitmap; //源图二绽放后输出的目标位图
  AContrast: Double; //对比度，默认0不调整
  AUseCache: Boolean//是否使用缓存空间，使用可以避免频繁申请释放内存，应注意线程安全
  ): Single;
begin
  Result := MatchFaceWithIEBitmaps(AInBitmap1, AInBitmap2, AOutIEBitmp1,
    AOutIEBitmp2, FResampleWidth, FResampleHeight, FResampleFilter, AContrast,
    AUseCache);
end;

//从文件中读取，支持所有ImageEN支持的格式
class function TArcFaceSDKIEVersion.ReadFromFile(
  AFileName: string; //文件名
  var AImgData: TImgdataInfo; //图像数据结构
  AOutIEBitmp: TIEBitmap; //缩放后输出的目标位图
  AResampleWidth: Integer = 0; //缩放宽度
  AResampleHeight: Integer = 0; //缩放高度
  AResampleFilter: TResampleFilter = rfNone; //绽放滤镜
  AContrast: Double = 0//调整对比度
  ): Boolean;
var
  lBitMap: TIEBitmap;
  io: TImageEnIO;
begin
  Result := False;
  if not FileExists(AFileName) then
    Exit;
  lBitMap := TIEBitmap.Create;
  io := TImageEnIO.Create(nil);
  try
    io.AttachedIEBitmap := lBitMap;
    io.LoadFromFile(AFileName);

    Result := ReadIEBitmap(lBitMap, AImgData, AOutIEBitmp,
      AResampleWidth, AResampleHeight, AResampleFilter, AContrast);

  finally
    io.Free;
    lBitMap.Free;
  end;
end;

//从IEBitmap中读取
class function TArcFaceSDKIEVersion.ReadIEBitmap(
  ABitmap: TIEBitmap; //源位图
  var AImgDataInfo: TImgdataInfo; //图像数据结构
  AOutIEBitmp: TIEBitmap; //缩放后输出的目标位图
  AResampleWidth: Integer = 0; //缩放宽度
  AResampleHeight: Integer = 0; //缩放高度
  AResampleFilter: TResampleFilter = rfNone; //绽放滤镜
  AContrast: Double = 0//调整对比度
  ): Boolean;
var
  iLineByte: Integer;
  iBitCount: Integer;
  i: Integer;
  lBitMap: TIEBitmap;
  proc: TImageEnProc;
  LastLP: Pointer;
  iNewWidth, iNewHeight: Integer;
begin
  Result := False;
  if ABitmap = nil then
    Exit;

  if (AResampleWidth <> 0) or (AResampleHeight <> 0) then
  begin
    lBitMap := TIEBitmap.Create;
    //复制数据
    lBitMap.Assign(ABitmap);
    proc := TImageEnProc.Create(nil);
    try
      proc.AttachedIEBitmap := lBitMap;
      //如果指定宽度和高度均大于0
      if (AResampleWidth > 0) and (AResampleHeight > 0) then
      begin
        //如果宽比>高比则以宽为基准进行调整，否则以高为基准进行调整，相当于宽优先
        if AResampleWidth / AResampleHeight > lBitMap.Width / lBitMap.Height
        then
        begin
          iNewWidth := AResampleWidth;
          iNewHeight := Round(AResampleWidth / lBitMap.Width * lBitMap.Height);
        end
        else
        begin
          iNewWidth := Round(AResampleHeight / lBitMap.Height * lBitMap.Width);
          iNewHeight := AResampleHeight;
        end;
        proc.Resample(iNewWidth, iNewHeight, AResampleFilter);
      end
      {
       else if (AResampleWidth < 0) and (AResampleHeight < 0) then
       begin
       if lBitMap.Width < Abs(AResampleWidth) then
       proc.Resample(Abs(AResampleWidth),
       Round(Abs(AResampleWidth) / lBitMap.Width *
       lBitMap.Height), AResampleFilter)
       else if lBitMap.Height < Abs(AResampleHeight) then
       proc.Resample(Round(Abs(AResampleHeight) / lBitMap.Height *
       lBitMap.Width), Abs(AResampleHeight), AResampleFilter)
       end
      }
      //如果指定宽小于0，则检查当前宽是否小于指定宽的绝对值，
      //如果小于的则以宽度为基准进行调整
      else if (AResampleWidth < 0) then
      begin
        if lBitMap.Width < Abs(AResampleWidth) then
          proc.Resample(Abs(AResampleWidth),
            Round(Abs(AResampleWidth) / lBitMap.Width *
            lBitMap.Height), AResampleFilter)
      end
      //如果指定高小于0，则检查当前高是否小于指定高的绝对值，
      //如果小于的则以高度为基准进行调整
      else if (AResampleHeight < 0) then
      begin
        if lBitMap.Height < Abs(AResampleHeight) then
          proc.Resample(Round(Abs(AResampleHeight) / lBitMap.Height *
            lBitMap.Width), Abs(AResampleHeight), AResampleFilter)
      end
      //如果指定宽大于0，则以宽度为基准进行调整
      else if AResampleWidth > 0 then
        proc.Resample(AResampleWidth, Round(AResampleWidth / lBitMap.Width *
          lBitMap.Height), AResampleFilter)
        //如果指定高大于0，则以高度为基准进行调整
      else if AResampleHeight > 0 then
        proc.Resample(Round(AResampleHeight / lBitMap.Height * lBitMap.Width),
          AResampleHeight, AResampleFilter);
      //调整对比度
      if AContrast > 0 then
        proc.Contrast(AContrast);

    finally
      proc.Free;
    end;

  end
  else
    lBitMap := ABitmap;

  Result := False;
  iBitCount := lBitMap.BitCount;
  if iBitCount = 0 then
    Exit;

  iLineByte := Trunc((lBitMap.Width * iBitCount / 8 + 3) / 4) * 4;

  if
    (AImgDataInfo.BitCount <> iBitCount) or
    (AImgDataInfo.Width <> lBitMap.Width) or
    (AImgDataInfo.Height <> lBitMap.Height) then
  begin

    AImgDataInfo.BitCount := iBitCount;
    AImgDataInfo.Width := lBitMap.Width;
    AImgDataInfo.Height := lBitMap.Height;
    AImgDataInfo.LineBytes := iLineByte;

    if AImgDataInfo.pImgData <> nil then
      FreeMem(AImgDataInfo.pImgData);
    GetMem(AImgDataInfo.pImgData, iLineByte * lBitMap.Height);
  end
  else
  begin
    if AImgDataInfo.pImgData = nil then
      GetMem(AImgDataInfo.pImgData, iLineByte * lBitMap.Height);
  end;

  {
   LastLP := lBitMap.ScanLine[lBitMap.Height - 1];
   for i := 0 to lBitMap.Height - 1 do
   begin
   CopyMemory(Pointer(AImgDataInfo.pImgData + i * iLineByte),
   Pointer(Int64(LastLP) - i * iLineByte), iLineByte);
   end;
  }

  for i := lBitMap.Height - 1 downto 0 do
  begin
    CopyMemory(Pointer(AImgDataInfo.pImgData + i * iLineByte),
      lBitMap.ScanLine[i], iLineByte);
  end;

  if AOutIEBitmp <> nil then
    AOutIEBitmp.Assign(lBitMap);

  if lBitMap <> ABitmap then
    lBitMap.Free;

  Result := true;
end;

//从IEBitmap中获取人脸位置、性别和年龄信息列表（追踪模式）
function TArcFaceSDKIEVersion.TrackFacesAndAgeGenderFromIEBitmap(
  ABitmap: TIEBitmap; //源位图
  var AFaceInfos: TList<TFaceBaseInfo>; //人脸基本信息
  AUseCache: Boolean//是否使用缓存空间，使用可以避免频繁申请释放内存，应注意线程安全
  ): Boolean;
var
  lImgDataInfo: TImgdataInfo;
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
  ArrFaceOrient: array of AFT_FSDK_OrientCode;
  ArrFaceRect: array of MRECT;
  R: MRESULT;
begin
  Result := False;

  if AFaceInfos = nil then
    AFaceInfos := TList<TFaceBaseInfo>.Create;

  if FFaceDetectionEngine = nil then
    Exit;

  if AUseCache then
    lImgDataInfo := FImgDataInfoCache
  else
    lImgDataInfo.pImgData := nil;

  if not ReadIEBitmap(ABitmap, lImgDataInfo, nil) then
    Exit;

  offInput.u32PixelArrayFormat := ASVL_PAF_RGB24_B8G8R8;
  FillChar(offInput.pi32Pitch, SizeOf(offInput.pi32Pitch), 0);
  FillChar(offInput.ppu8Plane, SizeOf(offInput.ppu8Plane), 0);

  offInput.i32Width := lImgDataInfo.Width;
  offInput.i32Height := lImgDataInfo.Height;

  offInput.ppu8Plane[0] := IntPtr(lImgDataInfo.pImgData);
  offInput.pi32Pitch[0] := lImgDataInfo.LineBytes;

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

  if not AUseCache then
  begin
    if lImgDataInfo.pImgData <> nil then
      FreeMem(lImgDataInfo.pImgData)
  end
  else
  begin
    FImgDataInfoCache := lImgDataInfo;
  end;

end;

//从IEBitmap中获取人脸位置、性别和年龄信息列表
function TArcFaceSDKIEVersion.DetectFacesAndAgeGenderFromIEBitmap(
  ABitmap: TIEBitmap; //源位图
  var AFaceInfos: TList<TFaceBaseInfo>; //人脸基本信息列表
  AOutIEBitmp: TIEBitmap; //缩放后输出的目标位图
  AResampleWidth, //缩放宽度
  AResampleHeight: Integer; //缩放高度
  AResampleFilter: TResampleFilter; //缩放滤镜
  AContrast: Double; //对比度，默认0不调整
  AUseCache: Boolean//是否使用缓存空间，使用可以避免频繁申请释放内存，应注意线程安全
  ): Boolean;

var
  lImgDataInfo: TImgdataInfo;
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
  ArrFaceOrient: array of AFD_FSDK_OrientCode;
  ArrFaceRect: array of MRECT;
begin
  Result := False;

  if AFaceInfos = nil then
    AFaceInfos := TList<TFaceBaseInfo>.Create;

  if FFaceDetectionEngine = nil then
    Exit;

  if AUseCache then
    lImgDataInfo := FImgDataInfoCache
  else
    lImgDataInfo.pImgData := nil;

  if not ReadIEBitmap(ABitmap, lImgDataInfo, AOutIEBitmp, AResampleWidth,
    AResampleHeight, AResampleFilter, AContrast) then
    Exit;

  offInput.u32PixelArrayFormat := ASVL_PAF_RGB24_B8G8R8;
  FillChar(offInput.pi32Pitch, SizeOf(offInput.pi32Pitch), 0);
  FillChar(offInput.ppu8Plane, SizeOf(offInput.ppu8Plane), 0);

  offInput.i32Width := lImgDataInfo.Width;
  offInput.i32Height := lImgDataInfo.Height;

  offInput.ppu8Plane[0] := IntPtr(lImgDataInfo.pImgData);
  offInput.pi32Pitch[0] := lImgDataInfo.LineBytes;

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
            FFaceAgeEngine, //[in]年龄评估引擎实例句柄
            @offInput, //[in]原始图像数据
            @lFaceRes_Age, //[in]人脸框信息
            lAgeRes//[out]年龄评估结果
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
            FFaceGenderEngine, //[in]性别评估引擎实例句柄
            @offInput, //[in]原始图像数据
            @lFaceRes_Gender, //[in]人脸框信息
            lGenderRes//[out]性别评估结果
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

  if not AUseCache then
  begin
    if lImgDataInfo.pImgData <> nil then
      FreeMem(lImgDataInfo.pImgData)
  end
  else
  begin
    FImgDataInfoCache := lImgDataInfo;
  end;

end;

//从TIEBitmap中获取人脸位置、性别和年龄信息列表,部分参数使用类实例属性值
function TArcFaceSDKIEVersion.DRAGfromIEBitmap(
  ABitmap: TIEBitmap; //源位图
  var AFaceInfos: TList<TFaceBaseInfo>; //人脸基本信息列表
  var AFaceModels: TFaceModels; //人脸特征集
  AOutIEBitmp: TIEBitmap; //缩放后输出的目标位图
  AUseCache: Boolean//是否使用缓存空间，使用可以避免频繁申请释放内存，应注意线程安全
  ): Boolean;
begin
  Result := DRAGfromIEBitmap(ABitmap, AFaceInfos, AFaceModels,
    AOutIEBitmp, FResampleWidth, FResampleHeight, FResampleFilter,
    FContrastRatio, AUseCache);
end;

//从图像文件中获取人脸位置、性别和年龄信息列表,部分参数使用类实例属性值
function TArcFaceSDKIEVersion.DRAGfromFile(
  AFileName: string; //图像文件名，支持 ImageEN 支持的所有图像格式
  var AFaceInfos: TList<TFaceBaseInfo>; //人脸基本信息列表
  var AFaceModels: TFaceModels; //人脸特征集
  AOutIEBitmp: TIEBitmap; //缩放后输出的目标位图
  AUseCache: Boolean//是否使用缓存空间，使用可以避免频繁申请释放内存，应注意线程安全
  ): Boolean;
begin
  Result := DRAGfromFile(AFileName, AFaceInfos, AFaceModels,
    AOutIEBitmp, FResampleWidth, FResampleHeight, FResampleFilter,
    FContrastRatio, AUseCache);
end;

//从图像文件中获取人脸位置、年龄、性别和特征信息列表
function TArcFaceSDKIEVersion.DRAGfromFile(
  AFileName: string; //图像文件名，支持 ImageEN 支持的所有图像格式
  var AFaceInfos: TList<TFaceBaseInfo>; //人脸基本信息列表
  var AFaceModels: TFaceModels; //人脸特征集
  AOutIEBitmp: TIEBitmap; //缩放后输出的目标位图
  AResampleWidth, //缩放宽度
  AResampleHeight: Integer; //缩放高度
  AResampleFilter: TResampleFilter; //缩放滤镜
  AContrast: Double; //对比度，默认0不调整
  AUseCache: Boolean//是否使用缓存空间，使用可以避免频繁申请释放内存，应注意线程安全
  ): Boolean;
var
  lIEBitmap: TIEBitmap;
  io: TImageEnIO;
begin
  Result := False;
  if not FileExists(AFileName) then
    Exit;

  lIEBitmap := TIEBitmap.Create;
  io := TImageEnIO.Create(nil);
  try
    io.AttachedIEBitmap := lIEBitmap;
    io.LoadFromFile(AFileName);
    Result := DRAGfromIEBitmap(lIEBitmap, AFaceInfos,
      AFaceModels, AOutIEBitmp, AResampleWidth, AResampleHeight,
      AResampleFilter, AContrast, AUseCache);
  finally
    lIEBitmap.Free;
    io.Free;
  end;
end;

end.
