(* ******************************************************
  * 虹软人脸识别SDK封装 ImageEN 版
  * 版权所有 (C) 2017 NJTZ  eMail:yhdgs@qq.com
  ****************************************************** *)
{$INCLUDE ARCFACE.INC}
unit ArcFaceSDKIEVersion;

interface

uses Windows, Messages, SysUtils, System.Classes, amcomdef, ammemDef,
  arcsoft_fsdk_face_detection,
  arcsoft_fsdk_face_recognition,
  arcsoft_fsdk_face_tracking, asvloffscreendef, merrorDef,
  arcsoft_fsdk_age_estimation, arcsoft_fsdk_gender_estimation,
  Vcl.Graphics, Vcl.Imaging.jpeg, System.Generics.Collections, ArcFaceSDK,
  hyieutils, hyiedefs, imageenio, imageenproc, imageenview, iexBitmaps,
  System.Math {$IFDEF ARC_RZ_SDK}, arcsoft_fsdk_fic{$ENDIF};

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
      ADrawIndex: Boolean = true; AZoomRotation: Double = 1; ATextSize: Integer =
      12; AAlphaBlend: Boolean = false; ASourceConstantAlpha: Integer = 180;
      ABlendColor: TColor = -1);
    class procedure DrawFaceRectEX(AView: TImageEnView; AFaceIdx: Integer;
      AFaceRect: MRECT; AColor: TColor = clBlue; AWidth: Integer = 2; ADrawIndex:
      Boolean = true; AZoomRotation: Double = 1; ATextSize: Integer = 12;
      AAlphaBlend: Boolean = false; ASourceConstantAlpha: Integer = 180;
      ABlendColor: TColor = -1);
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
{$IFDEF ARC_RZ_SDK}
    function RzFicFaceDataFeatureExtractionFromIEBitmap(ABitmap: TIEBitmap;
      isVideo: Boolean; AOutIEBitmp: TIEBitmap; AResampleWidth, AResampleHeight:
      Integer; AResampleFilter: TResampleFilter; AContrast: Double; AUseCache:
      Boolean): Boolean; overload;
    function RzFicFaceDataFeatureExtractionFromIEBitmap(ABitmap: TIEBitmap;
      isVideo: Boolean; var AFaceRes: AFIC_FSDK_FACERES; AOutIEBitmp: TIEBitmap;
      AResampleWidth, AResampleHeight: Integer; AResampleFilter: TResampleFilter;
      AContrast: Double; AUseCache: Boolean): Boolean; overload;
    function RzFicFaceDataFeatureExtractionFromFile(AFile: String; isVideo:
      Boolean; AOutIEBitmp: TIEBitmap; AResampleWidth, AResampleHeight: Integer;
      AResampleFilter: TResampleFilter; AContrast: Double; AUseCache: Boolean):
      Boolean; overload;
    function RzFicFaceDataFeatureExtractionFromFile(AFile: String; isVideo:
      Boolean; var AFaceRes: AFIC_FSDK_FACERES; AOutIEBitmp: TIEBitmap;
      AResampleWidth, AResampleHeight: Integer; AResampleFilter: TResampleFilter;
      AContrast: Double; AUseCache: Boolean): Boolean; overload;
    // 1 人证比对（从内存载入）
    function RzFicCompareFromIEBitmap(AZjz, ARyZP: TIEBitmap; isVideo: Boolean; var
      ASimilarScore: Single; var ACompareResult: Integer; AThreshold: Single =
      0.82): Boolean; overload;
    // 1 人证比对（从内存载入）
    function RzFicCompareFromIEBitmap(AZjz, ARyZP: TIEBitmap; isVideo: Boolean; var
      ASimilarScore: Single; var ACompareResult: Integer; var AFaceRes:
      AFIC_FSDK_FACERES; AThreshold: Single = 0.82): Boolean; overload;
    // 1 人证比对（从文件载入）
    function RzFicCompareFromFile(AZjz, ARyZP: string; isVideo: Boolean; var
      ASimilarScore: Single; var ACompareResult: Integer; AThreshold: Single =
      0.82): Boolean; overload;
    // 1 人证比对（从文件载入）
    function RzFicCompareFromFile(AZjz, ARyZP: string; isVideo: Boolean; var
      ASimilarScore: Single; var ACompareResult: Integer; var AFaceRes:
      AFIC_FSDK_FACERES; AThreshold: Single = 0.82): Boolean; overload;
    function RzFicIdCardDataFeatureExtractionFromIEBitmap(ABitmap, AOutIEBitmp:
      TIEBitmap; AResampleWidth, AResampleHeight: Integer; AResampleFilter:
      TResampleFilter; AContrast: Double): Boolean;
    function RzFicIdCardDataFeatureExtractionFromFile(AFile: String; AOutIEBitmp:
      TIEBitmap; AResampleWidth, AResampleHeight: Integer; AResampleFilter:
      TResampleFilter; AContrast: Double): Boolean;
{$ENDIF}
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
  inherited;
  if FImgDataInfoCache.pImgData <> nil then
    FreeMem(FImgDataInfoCache.pImgData);
end;

//

{ -------------------------------------------------------------------------------
  过程名:    TArcFaceSDKIEVersion.CropFace
  作者:      NJTZ
  日期:      2018.09.22
  功能:      根据人脸框信息裁剪人脸到新文件
  参数:
  ASouceIEBitmap, // 源BMP
  ADestBitmap: TIEBitmap; // 目标BMP
  AFaceRegion: AFR_FSDK_FACEINPUT; // 人脸区域
  AExtendRatio, // 扩展比率，最小为0，实际应用再除以100
  AResampleWidth, // 目标图像宽度,0表示不变或随高度变动
  AResampleHeight: Integer; // 目标图像高度,0表示不变或随宽度变动
  AResampleFilter: TResampleFilter // 缩放滤镜
  返回值:    Boolean
  ------------------------------------------------------------------------------- }
class function TArcFaceSDKIEVersion.CropFace(
  ASouceIEBitmap,
  ADestBitmap: TIEBitmap;
  AFaceRegion: AFR_FSDK_FACEINPUT;
  AExtendRatio,
  AResampleWidth,
  AResampleHeight: Integer;
  AResampleFilter: TResampleFilter
  ): Boolean;
var
  iLeft, iTop, iRight, iBottom, iWidthExtend, iHeightExtend, iTmp: Integer;
  fHvsW: Double;
  iWidth, iHeight: Integer;
begin
  Result := false;
  if ASouceIEBitmap = nil then
    Exit;

  // 图像高宽比，如果缩放高宽其中有一为0表示维持比率不变
  if (AResampleWidth <= 0) or (AResampleHeight <= 0) then
    fHvsW := 0
  else
    fHvsW := AResampleHeight / AResampleWidth;

  // 图像从人脸框向四周扩散的像素
  iWidthExtend := Round((AFaceRegion.rcFace.right - AFaceRegion.rcFace.left) *
    (AExtendRatio / 100));
  iHeightExtend :=
    Round((AFaceRegion.rcFace.bottom - AFaceRegion.rcFace.top) *
    (AExtendRatio / 100));

  // 向四周扩展
  iLeft := AFaceRegion.rcFace.left - iWidthExtend;
  iRight := AFaceRegion.rcFace.right + iWidthExtend;
  iTop := AFaceRegion.rcFace.top - iHeightExtend;
  iBottom := AFaceRegion.rcFace.bottom + iHeightExtend;

  // 临时宽高
  iWidth := Abs(iRight - iLeft);
  iHeight := Abs(iBottom - iTop);

  // 如果 高/宽 > 1，则宽度保持不变，再次扩展高度
  if fHvsW > 1 then
  begin
    iTmp := (Max(Round(iWidth * fHvsW), iHeight) - iHeight) div 2;
    // 重新定为顶、底
    iTop := iTop - iTmp;
    iBottom := iBottom + iTmp;
  end
  // 如果 0 < 高/宽 <= 1，则高度保持不变，再次扩展宽度
  else if fHvsW > 0 then
  begin
    iTmp := (Max(Round(iHeight / fHvsW), iWidth) - iWidth) div 2;
    iLeft := iLeft - iTmp;
    iRight := iRight + iTmp;
  end;

  // 如果顶小于0并且底大于原始图像高度
  if (iTop < 0) and (iBottom > ASouceIEBitmap.Height) then
  begin
    // 顶置0
    iTop := 0;
    // 底置原始图像高度
    iBottom := ASouceIEBitmap.Height;
  end
  // 仅顶小于0
  else if iTop < 0 then
  begin
    // 扩展底
    iBottom := Min(iBottom - iTop, ASouceIEBitmap.Height);
    iTop := 0;
  end
  // 仅底大于原始图像高度
  else if iBottom > ASouceIEBitmap.Height then
  begin
    // 扩展顶
    iTop := Max(iTop - (iBottom - ASouceIEBitmap.Height), 0);
    iBottom := ASouceIEBitmap.Height;
  end;

  // 如果左边小于0并且右边大于原始图像宽度
  if (iLeft < 0) and (iRight > ASouceIEBitmap.Width) then
  begin
    iLeft := 0;
    iRight := ASouceIEBitmap.Width;
  end
  // 仅左边小于0
  else if iLeft < 0 then
  begin
    iRight := Min(iRight - iLeft, ASouceIEBitmap.Width);
    iLeft := 0;
  end
  // 仅右边大于原始图像宽度
  else if iRight > ASouceIEBitmap.Width then
  begin
    iLeft := Max(iLeft - (iRight - ASouceIEBitmap.Width), 0);
    iRight := ASouceIEBitmap.Width;
  end;

  if ADestBitmap = nil then
    ADestBitmap := TIEBitmap.Create;

  // 设置目标图像宽高
  ADestBitmap.Width := Abs(iRight - iLeft);
  ADestBitmap.Height := Abs(iBottom - iTop);

  // 拷贝图像
  ASouceIEBitmap.CopyRectTo(ADestBitmap, iLeft, iTop, 0, 0, Abs(iRight - iLeft),
    Abs(iBottom - iTop));
end;

{ -------------------------------------------------------------------------------
  过程名:    TArcFaceSDKIEVersion.DetectAndRecognitionFacesFromIEBitmap
  作者:      NJTZ
  日期:      2018.09.22
  功能:      从IEBitmap中获取人脸位置和特征信息列表
  参数:
  ABitmap: TIEBitmap; // 源位图
  var AFaceRegions: TList<AFR_FSDK_FACEINPUT>; // 人脸框信息列表
  var AFaceModels: TFaceModels; // 人脸特征集
  AOutIEBitmp: TIEBitmap; // 缩放后输出的目标位图
  AResampleWidth, // 缩放宽度
  AResampleHeight: Integer; // 绽放高度
  AResampleFilter: TResampleFilter; // 缩放滤镜
  AContrast: Double; // 对比度，默认0不调整
  AUseCache: Boolean // 是否使用缓存空间，可以避免频繁申请释放内存，应注意线程安全
  返回值:    Boolean
  ------------------------------------------------------------------------------- }
function TArcFaceSDKIEVersion.DetectAndRecognitionFacesFromIEBitmap(
  ABitmap: TIEBitmap;
  var AFaceRegions: TList<AFR_FSDK_FACEINPUT>;
  var AFaceModels: TFaceModels;
  AOutIEBitmp: TIEBitmap;
  AResampleWidth,
  AResampleHeight: Integer;
  AResampleFilter: TResampleFilter;
  AContrast: Double;
  AUseCache: Boolean
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
  Result := false;

  if FFaceDetectionEngine = nil then
    Exit;

  if AFaceRegions = nil then
    AFaceRegions := TList<AFR_FSDK_FACEINPUT>.Create;

  // 使用缓存
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
  // 人脸检测
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

{ -------------------------------------------------------------------------------
  过程名:    TArcFaceSDKIEVersion.DetectAndRecognitionFacesFromIEBitmap
  作者:      NJTZ
  日期:      2018.09.22
  功能:      从IEBitmap中获取人脸位置和特征信息列表，缩放参数使用本对象实例的缩放参数属性
  ResampleWidth、ResampleHeight、ResampleFilter等参数使用类实例相应属性
  参数:
  ABitmap: TIEBitmap; // 源位图
  var AFaceRegions: TList<AFR_FSDK_FACEINPUT>; // 人脸框信息列表
  var AFaceModels: TFaceModels; // 人脸特征集
  AOutIEBitmp: TIEBitmap; // 缩放后输出的目标位图
  AUseCache: Boolean // 是否使用缓存空间，可以避免频繁申请释放内存，应注意线程安全
  返回值:    Boolean
  ------------------------------------------------------------------------------- }
function TArcFaceSDKIEVersion.DetectAndRecognitionFacesFromIEBitmap(
  ABitmap: TIEBitmap;
  var AFaceRegions: TList<AFR_FSDK_FACEINPUT>;
  var AFaceModels: TFaceModels;
  AOutIEBitmp: TIEBitmap;
  AUseCache: Boolean
  ): Boolean;
begin
  Result := DetectAndRecognitionFacesFromIEBitmap(ABitmap, AFaceRegions,
    AFaceModels, AOutIEBitmp, FResampleWidth, FResampleHeight, FResampleFilter,
    FContrastRatio, AUseCache);
end;

{ -------------------------------------------------------------------------------
  过程名:    TArcFaceSDKIEVersion.DetectAndRecognitionFacesFromBitmapEX
  作者:      NJTZ
  日期:      2018.09.22
  功能:      从标准Bitmap获取人脸位置和特征信息列表
  参数:
  ABitmap: TBitmap; // 源位图
  var AFaceRegions: TList<AFR_FSDK_FACEINPUT>; // 人脸框信息列表
  var AFaceModels: TFaceModels; // 人脸特征集
  AOutIEBitmp: TIEBitmap; // 缩放后输出的目标位图
  AResampleWidth, // 缩放宽度
  AResampleHeight: Integer; // 缩放高度
  AResampleFilter: TResampleFilter; // 缩放滤镜
  AContrast: Double; // 对比度，默认0不调整
  AUseCache: Boolean // 是否使用缓存空间，使用可以避免频繁申请释放内存，应注意线程安全
  返回值:    Boolean
  ------------------------------------------------------------------------------- }
function TArcFaceSDKIEVersion.DetectAndRecognitionFacesFromBitmapEX(
  ABitmap: TBitmap;
  var AFaceRegions: TList<AFR_FSDK_FACEINPUT>;
  var AFaceModels: TFaceModels;
  AOutIEBitmp: TIEBitmap;
  AResampleWidth,
  AResampleHeight: Integer;
  AResampleFilter: TResampleFilter;
  AContrast: Double;
  AUseCache: Boolean
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

//

{ -------------------------------------------------------------------------------
  过程名:    TArcFaceSDKIEVersion.DetectFacesAndAgeGenderFromBitmapEX
  作者:      NJTZ
  日期:      2018.09.22
  功能:      从标准位图中获取人脸位置、性别和年龄信息列表
  参数:
  ABitmap: TBitmap; // 源位图
  var AFaceInfos: TList<TFaceBaseInfo>; // 人脸基本信息列表
  AOutIEBitmp: TIEBitmap; // 缩放后输出的目标位图
  AResampleWidth, // 缩放宽度
  AResampleHeight: Integer; // 缩放高度
  AResampleFilter: TResampleFilter; // 缩放滤镜
  AContrast: Double; // 对比度，默认0不调整
  AUseCache: Boolean // 是否使用缓存空间，使用可以避免频繁申请释放内存，应注意线程安全
  返回值:    Boolean
  ------------------------------------------------------------------------------- }
function TArcFaceSDKIEVersion.DetectFacesAndAgeGenderFromBitmapEX(
  ABitmap: TBitmap;
  var AFaceInfos: TList<TFaceBaseInfo>;
  AOutIEBitmp: TIEBitmap;
  AResampleWidth,
  AResampleHeight: Integer;
  AResampleFilter: TResampleFilter;
  AContrast: Double;
  AUseCache: Boolean
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

{ -------------------------------------------------------------------------------
  过程名:    TArcFaceSDKIEVersion.DetectAndRecognitionFacesFromFileEx
  作者:      NJTZ
  日期:      2018.09.22
  功能:      从图像文件中检测人脸位置信息列表并提取所有检测到的人脸特征信息,支持常见格式图像文件
  参数:
  AFileName: string; // 文件名
  var AFaceRegions: TList<AFR_FSDK_FACEINPUT>; // 人脸框信息列表
  var AFaceModels: TFaceModels; // 人脸特征集
  AOutIEBitmp: TIEBitmap; // 缩放后输出的目标位图
  AResampleWidth, // 缩放宽度
  AResampleHeight: Integer; // 缩放高度
  AResampleFilter: TResampleFilter; // 缩放滤镜
  AContrast: Double; // 对比度，默认0不调整
  AUseCache: Boolean // 是否使用缓存空间，使用可以避免频繁申请释放内存，应注意线程安全
  返回值:    Boolean
  ------------------------------------------------------------------------------- }
function TArcFaceSDKIEVersion.DetectAndRecognitionFacesFromFileEx(
  AFileName: string;
  var AFaceRegions: TList<AFR_FSDK_FACEINPUT>;
  var AFaceModels: TFaceModels;
  AOutIEBitmp: TIEBitmap;
  AResampleWidth,
  AResampleHeight: Integer;
  AResampleFilter: TResampleFilter;
  AContrast: Double;
  AUseCache: Boolean
  ): Boolean;
var
  lIEBitmap: TIEBitmap;
  io: TImageEnIO;
begin
  Result := false;
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

{ -------------------------------------------------------------------------------
  过程名:    TArcFaceSDKIEVersion.DRAGfromIEBitmap
  作者:      NJTZ
  日期:      2018.09.22
  功能:      从IEBitmap中获取人脸位置、年龄、性别和特征信息列表
  参数:
  ABitmap: TIEBitmap; // 源位图
  var AFaceInfos: TList<TFaceBaseInfo>; // 人脸基本信息列表
  var AFaceModels: TFaceModels; // 人脸特征集
  AOutIEBitmp: TIEBitmap; // 缩放后输出的目标位图
  AResampleWidth, // 缩放宽度
  AResampleHeight: Integer; // 缩放高度
  AResampleFilter: TResampleFilter; // 缩放滤镜
  AContrast: Double; // 对比度，默认0不调整
  AUseCache: Boolean // 是否使用缓存空间，使用可以避免频繁申请释放内存，应注意线程安全
  返回值:    Boolean
  ------------------------------------------------------------------------------- }
function TArcFaceSDKIEVersion.DRAGfromIEBitmap(
  ABitmap: TIEBitmap;
  var AFaceInfos: TList<TFaceBaseInfo>;
  var AFaceModels: TFaceModels;
  AOutIEBitmp: TIEBitmap;
  AResampleWidth,
  AResampleHeight: Integer;
  AResampleFilter: TResampleFilter;
  AContrast: Double;
  AUseCache: Boolean
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
  Result := false;

  if AFaceInfos = nil then
    AFaceInfos := TList<TFaceBaseInfo>.Create;
  if AFaceModels = nil then
    AFaceModels := TFaceModels.Create;

  if FFaceDetectionEngine = nil then
    Exit;

  // 使用缓存
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
  // 人脸检测
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
        // ===================================================
        // 检测年龄
        // ===================================================
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
            FFaceAgeEngine, // [in] age estimation engine
            @offInput, // [in] the original image information
            // [in] the face rectangles information
            @lFaceRes_Age,
            // [out] the results of age estimation
            lAgeRes
            ) = MOK then
            // 分解人脸年龄
            ExtractFaceAges(lAgeRes, lAges)
          else
            Result := false;
{$IFDEF DEBUG}
          DoLog('检测年龄耗时：' + IntToStr(GetTickCount - T));
{$ENDIF}
        end
        else
          Result := false;

        // ===================================================
        // 检测性别
        // ===================================================
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
            FFaceGenderEngine, // [in] Gender estimation engine
            @offInput, // [in] the original imGender information
            // [in] the face rectangles information
            @lFaceRes_Gender,
            // [out] the results of Gender estimation
            lGenderRes
            ) = MOK then
            // 分解人脸性别
            ExtractFaceGenders(lGenderRes, lGenders)
          else
            Result := false;
{$IFDEF DEBUG}
          DoLog('检测性别耗时：' + IntToStr(GetTickCount - T));
{$ENDIF}
        end
        else
          Result := false;

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


        // ===================================================
        // 提取特征
        // ===================================================

{$IFDEF DEBUG}
        T := GetTickCount;
{$ENDIF}
        if not ExtractFaceFeatures(offInput, lFaceRegions, AFaceModels) then
          Result := false;
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

{ -------------------------------------------------------------------------------
  过程名:    TArcFaceSDKIEVersion.DetectFacesAndAgeGenderFromIEBitmap
  作者:      NJTZ
  日期:      2018.09.22
  功能:      // 从TIEBitmap中获取人脸位置、性别和年龄信息列表,部分参数使用类实例属性
  参数:
  ABitmap: TIEBitmap; // 源位图
  var AFaceInfos: TList<TFaceBaseInfo>; // 人脸基本信息列表
  AOutIEBitmp: TIEBitmap; // 缩放后输出的目标位图
  AUseCache: Boolean // 是否使用缓存空间，使用可以避免频繁申请释放内存，应注意线程安全
  返回值:    Boolean
  ------------------------------------------------------------------------------- }
function TArcFaceSDKIEVersion.DetectFacesAndAgeGenderFromIEBitmap(
  ABitmap: TIEBitmap;
  var AFaceInfos: TList<TFaceBaseInfo>;
  AOutIEBitmp: TIEBitmap;
  AUseCache: Boolean
  ): Boolean;
begin
  Result := DetectFacesAndAgeGenderFromIEBitmap(ABitmap, AFaceInfos,
    AOutIEBitmp, FResampleWidth, FResampleHeight, FResampleFilter,
    FContrastRatio, AUseCache);
end;

{ -------------------------------------------------------------------------------
  过程名:    TArcFaceSDKIEVersion.DetectFacesAndAgeGenderFromFileEx
  作者:      NJTZ
  日期:      2018.09.22
  功能:      从图像文件中获取人脸位置、性别和年龄信息列表
  参数:
  AFileName: string; //图像文件名
  var AFaceInfos: TList<TFaceBaseInfo>; //人脸基本信息列表
  AOutIEBitmp: TIEBitmap; //缩放后输出的目标位图
  AResampleWidth, //缩放宽度
  AResampleHeight: Integer; //缩放高度
  AResampleFilter: TResampleFilter; //缩放滤镜
  AContrast: Double; //对比度，默认0不调整
  AUseCache: Boolean//是否使用缓存空间，使用可以避免频繁申请释放内存，应注意线程安全
  返回值:    Boolean
  ------------------------------------------------------------------------------- }
function TArcFaceSDKIEVersion.DetectFacesAndAgeGenderFromFileEx(
  AFileName: string;
  var AFaceInfos: TList<TFaceBaseInfo>;
  AOutIEBitmp: TIEBitmap;
  AResampleWidth,
  AResampleHeight: Integer;
  AResampleFilter: TResampleFilter;
  AContrast: Double;
  AUseCache: Boolean
  ): Boolean;
var
  lIEBitmap: TIEBitmap;
  io: TImageEnIO;
begin
  Result := false;
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

// 从IEBitmam中获取人脸位置信息列表
function TArcFaceSDKIEVersion.DetectFacesFromIEBitmap(ABitmap: TIEBitmap; var
  AFaceRegions: TList<AFR_FSDK_FACEINPUT>; AOutIEBitmp: TIEBitmap;
  AResampleWidth, AResampleHeight: Integer; AResampleFilter: TResampleFilter;
  AContrast: Double; AUseCache: Boolean): Boolean;
var
  offInput: ASVLOFFSCREEN;
  pFaceRes: LPAFD_FSDK_FACERES;
  lImgData: TImgdataInfo;
begin
  Result := false;

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
  // 人脸检测
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

{ -------------------------------------------------------------------------------
  过程名:    TArcFaceSDKIEVersion.DetectFacesFromFile
  作者:      NJTZ
  日期:      2018.09.22
  功能:      从图像文件中获取人脸位置信息列表
  参数:
  AFile: string; // 图像文件名
  var AFaceRegions: TList<AFR_FSDK_FACEINPUT>; // 人脸框信息列表
  AOutIEBitmp: TIEBitmap; // 缩放后输出的目标位图
  AResampleWidth, // 缩放宽度
  AResampleHeight: Integer; // 缩放高度
  AResampleFilter: TResampleFilter; // 缩放滤镜
  AContrast: Double; // 对比度，默认0不调整
  AUseCache: Boolean // 是否使用缓存空间，使用可以避免频繁申请释放内存，应注意线程安全
  返回值:    Boolean
  ------------------------------------------------------------------------------- }
function TArcFaceSDKIEVersion.DetectFacesFromFile(
  AFile: string;
  var AFaceRegions: TList<AFR_FSDK_FACEINPUT>;
  AOutIEBitmp: TIEBitmap;
  AResampleWidth,
  AResampleHeight: Integer;
  AResampleFilter: TResampleFilter;
  AContrast: Double;
  AUseCache: Boolean
  ): Boolean;
var
  lImgData: TImgdataInfo;
  offInput: ASVLOFFSCREEN;
  pFaceRes: LPAFD_FSDK_FACERES;
begin
  Result := false;

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

  // 人脸检测
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

// 从文件中获取人脸位置信息列表，部分参数使用类实例属性值
function TArcFaceSDKIEVersion.DetectFacesFromFile(
  AFile: string; // 图像文件
  var AFaceRegions: TList<AFR_FSDK_FACEINPUT>; // 人脸框信息列表
  AOutIEBitmp: TIEBitmap; // 缩放后输出目标位图
  AUseCache: Boolean // 是否使用缓存空间
  ): Boolean;
begin
  Result := DetectFacesFromFile(AFile, AFaceRegions, AOutIEBitmp,
    FResampleWidth, FResampleHeight, FResampleFilter, FContrastRatio,
    AUseCache);
end;

{ -------------------------------------------------------------------------------
  过程名:    TArcFaceSDKIEVersion.DetectFacesFromIEBitmap
  作者:      NJTZ
  日期:      2018.09.22
  功能:      从IEBitmap中获取人脸位置信息列表，部分参数使用类实例属性值
  参数:
  ABitmap: TIEBitmap; // 源位图
  var AFaceRegions: TList<AFR_FSDK_FACEINPUT>; // 人脸框信息列表
  AOutIEBitmp: TIEBitmap; // 缩放后输出的目标位图
  AUseCache: Boolean // 是否使用缓存空间，使用可以避免频繁申请释放内存，应注意线程安全
  返回值:    Boolean
  ------------------------------------------------------------------------------- }
function TArcFaceSDKIEVersion.DetectFacesFromIEBitmap(
  ABitmap: TIEBitmap;
  var AFaceRegions: TList<AFR_FSDK_FACEINPUT>;
  AOutIEBitmp: TIEBitmap;
  AUseCache: Boolean
  ): Boolean;
begin
  Result := DetectFacesFromIEBitmap(ABitmap, AFaceRegions, AOutIEBitmp,
    FResampleWidth, FResampleHeight, FResampleFilter, FContrastRatio,
    AUseCache);
end;

{ -------------------------------------------------------------------------------
  过程名:    TArcFaceSDKIEVersion.TrackFacesFromIEBitmap
  作者:      NJTZ
  日期:      2018.09.22
  功能:      从IEBitmap中获取人脸位置信息列表（追踪模式），部分参数使用类实例属性值
  参数:
  ABitmap: TIEBitmap; // 源位图
  var AFaceRegions: TList<AFR_FSDK_FACEINPUT>; // 人脸框信息列表
  AUseCache: Boolean // 是否使用缓存空间，使用可以避免频繁申请释放内存，应注意线程安全
  返回值:    Boolean
  ------------------------------------------------------------------------------- }
function TArcFaceSDKIEVersion.TrackFacesFromIEBitmap(
  ABitmap: TIEBitmap;
  var AFaceRegions: TList<AFR_FSDK_FACEINPUT>;
  AUseCache: Boolean
  ): Boolean;
var
  lImgDataInfo: TImgdataInfo;
  offInput: ASVLOFFSCREEN;
  pFaceRes: LPAFT_FSDK_FACERES;
begin
  Result := false;

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

  // 人脸检测
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

{ -------------------------------------------------------------------------------
  过程名:    TArcFaceSDKIEVersion.DrawFaceRectAgeGenderEX
  作者:      NJTZ
  日期:      2018.09.22
  功能:      在ImageEnView上画人脸框、性别、年龄
  参数:
  AView: TImageEnView; // 图像预览组件
  AFaceIdx: Integer; // 人脸索引
  AFaceInfo: TFaceBaseInfo; // 人脸基本信息
  AColor: TColor = clBlue; // 画笔颜色
  AWidth: Integer = 2; // 框线宽度
  ADrawIndex: Boolean = true; // 是否画索引
  ATextSize: Integer = 0 // 字符大小
  返回值:    无
  ------------------------------------------------------------------------------- }
class procedure TArcFaceSDKIEVersion.DrawFaceRectAgeGenderEX(AView:
  TImageEnView; AFaceIdx: Integer; AFaceInfo: TFaceBaseInfo; AColor: TColor =
  clBlue; AWidth: Integer = 2; ADrawIndex: Boolean = true; AZoomRotation:
  Double = 1; ATextSize: Integer = 12; AAlphaBlend: Boolean = false;
  ASourceConstantAlpha: Integer = 180; ABlendColor: TColor = -1);
begin
  DrawFaceRectAgeGender(AView.IEBitmap.Canvas, AFaceIdx, AFaceInfo, AColor,
    AWidth, ADrawIndex, AZoomRotation, ATextSize, AAlphaBlend,
    ASourceConstantAlpha, ABlendColor);
  AView.Update;
end;

{ -------------------------------------------------------------------------------
  过程名:    TArcFaceSDKIEVersion.DrawFaceRectEX
  作者:      NJTZ
  日期:      2018.09.22
  功能:      在ImageEnView上画人脸框
  参数:
  AView: TImageEnView; // 图像预览组件
  AFaceIdx: Integer; // 人脸索引
  AFaceRect: AFR_FSDK_FACEINPUT; // 人脸框信息
  AColor: TColor = clBlue; // 画笔颜色
  AWidth: Integer = 2; // 框线宽度
  ADrawIndex: Boolean = true; // 是否画索引
  ATextSize: Integer = 0 // 文字大小
  返回值:    无
  ------------------------------------------------------------------------------- }
class procedure TArcFaceSDKIEVersion.DrawFaceRectEX(AView: TImageEnView;
  AFaceIdx: Integer; AFaceRect: MRECT; AColor: TColor = clBlue; AWidth:
  Integer = 2; ADrawIndex: Boolean = true; AZoomRotation: Double = 1;
  ATextSize: Integer = 12; AAlphaBlend: Boolean = false;
  ASourceConstantAlpha: Integer = 180; ABlendColor: TColor = -1);
begin
  DrawFaceRect(AView.IEBitmap.Canvas, AFaceIdx, AFaceRect, AColor, AWidth,
    ADrawIndex, AZoomRotation, ATextSize, AAlphaBlend, ASourceConstantAlpha,
    ABlendColor);
  AView.Update;
end;

{ -------------------------------------------------------------------------------
  过程名:    TArcFaceSDKIEVersion.ExtractFaceFeatureFromIEBitmap
  作者:      NJTZ
  日期:      2018.09.22
  功能:      从IEBitmap中提取单个人脸特征
  参数:
  AIEBitmap: TIEBitmap; // 源位图
  AFaceRegion: AFR_FSDK_FACEINPUT; // 人脸框信息
  var AFaceModel: AFR_FSDK_FACEMODEL; // 人脸特征，特征数据内存需手动使用freemem释放
  AOutIEBitmp: TIEBitmap; // 缩放后输出的目标位图
  AResampleWidth, // 缩放宽度
  AResampleHeight: Integer; // 缩放高度
  AResampleFilter: TResampleFilter; // 缩放滤镜
  AUseCache: Boolean // 是否使用缓存空间，使用可以避免频繁申请释放内存，应注意线程安全
  返回值:    Boolean
  ------------------------------------------------------------------------------- }
function TArcFaceSDKIEVersion.ExtractFaceFeatureFromIEBitmap(
  AIEBitmap: TIEBitmap; // 源位图
  AFaceRegion: AFR_FSDK_FACEINPUT; // 人脸框信息
  var AFaceModel: AFR_FSDK_FACEMODEL; // 人脸特征，特征数据内存需手动使用freemem释放
  AOutIEBitmp: TIEBitmap; // 缩放后输出的目标位图
  AResampleWidth, // 缩放宽度
  AResampleHeight: Integer; // 缩放高度
  AResampleFilter: TResampleFilter; // 缩放滤镜
  AUseCache: Boolean // 是否使用缓存空间，使用可以避免频繁申请释放内存，应注意线程安全
  ): Boolean;
var
  lImgData: TImgdataInfo;
  offInput: ASVLOFFSCREEN;
  pFaceRes: LPAFD_FSDK_FACERES;
begin
  Result := false;

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

  // 人脸特征提取
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

{ -------------------------------------------------------------------------------
  过程名:    TArcFaceSDKIEVersion.MatchFaceWithIEBitmaps
  作者:      NJTZ
  日期:      2018.09.22
  功能:      比对两张人脸IEBitmap（只比对第一个人脸）
  参数:
  AInBitmap1, // 待比较源图一
  AInBitmap2, // 待比较源图二
  AOutIEBitmp1, // 源图一缩放后输出的目标位图
  AOutIEBitmp2: TIEBitmap; // 源图二绽放后输出的目标位图
  AResampleWidth, // 缩放宽度
  AResampleHeight: Integer; // 缩放高度
  AResampleFilter: TResampleFilter; // 缩放滤镜
  AContrast: Double; // 对比度，默认0不调整
  AUseCache: Boolean // 是否使用缓存空间，使用可以避免频繁申请释放内存，应注意线程安全
  返回值:    Single
  ------------------------------------------------------------------------------- }
function TArcFaceSDKIEVersion.MatchFaceWithIEBitmaps(
  AInBitmap1, // 待比较源图一
  AInBitmap2, // 待比较源图二
  AOutIEBitmp1, // 源图一缩放后输出的目标位图
  AOutIEBitmp2: TIEBitmap; // 源图二绽放后输出的目标位图
  AResampleWidth, // 缩放宽度
  AResampleHeight: Integer; // 缩放高度
  AResampleFilter: TResampleFilter; // 缩放滤镜
  AContrast: Double; // 对比度，默认0不调整
  AUseCache: Boolean // 是否使用缓存空间，使用可以避免频繁申请释放内存，应注意线程安全
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

{ -------------------------------------------------------------------------------
  过程名:    TArcFaceSDKIEVersion.MatchFaceWithIEBitmaps
  作者:      NJTZ
  日期:      2018.09.22
  功能:      比对两张人脸IEBitmap（只比对第一个人脸），部分参数使用类实例的属性值
  参数:
  AInBitmap1, // 待比较源图一
  AInBitmap2, // 待比较源图二
  AOutIEBitmp1, // 源图一缩放后输出的目标位图
  AOutIEBitmp2: TIEBitmap; // 源图二绽放后输出的目标位图
  AContrast: Double; // 对比度，默认0不调整
  AUseCache: Boolean // 是否使用缓存空间，使用可以避免频繁申请释放内存，应注意线程安全
  返回值:    Single
  ------------------------------------------------------------------------------- }
function TArcFaceSDKIEVersion.MatchFaceWithIEBitmaps(
  AInBitmap1,
  AInBitmap2,
  AOutIEBitmp1,
  AOutIEBitmp2: TIEBitmap;
  AContrast: Double;
  AUseCache: Boolean
  ): Single;
begin
  Result := MatchFaceWithIEBitmaps(AInBitmap1, AInBitmap2, AOutIEBitmp1,
    AOutIEBitmp2, FResampleWidth, FResampleHeight, FResampleFilter, AContrast,
    AUseCache);
end;

// 从文件中读取，支持所有ImageEN支持的格式
class function TArcFaceSDKIEVersion.ReadFromFile(
  AFileName: string; // 文件名
  var AImgData: TImgdataInfo; // 图像数据结构
  AOutIEBitmp: TIEBitmap; // 缩放后输出的目标位图
  AResampleWidth: Integer = 0; // 缩放宽度
  AResampleHeight: Integer = 0; // 缩放高度
  AResampleFilter: TResampleFilter = rfNone; // 绽放滤镜
  AContrast: Double = 0 // 调整对比度
  ): Boolean;
var
  lBitMap: TIEBitmap;
  io: TImageEnIO;
begin
  Result := false;
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

{ -------------------------------------------------------------------------------
  过程名:    TArcFaceSDKIEVersion.ReadIEBitmap
  作者:      NJTZ
  日期:      2018.09.22
  功能:      从IEBitmap中读取
  参数:
  ABitmap: TIEBitmap; // 源位图
  var AImgDataInfo: TImgdataInfo; // 图像数据结构
  AOutIEBitmp: TIEBitmap; // 缩放后输出的目标位图
  AResampleWidth: Integer = 0; // 缩放宽度
  AResampleHeight: Integer = 0; // 缩放高度
  AResampleFilter: TResampleFilter = rfNone; // 绽放滤镜
  AContrast: Double = 0 // 调整对比度
  返回值:    Boolean
  ------------------------------------------------------------------------------- }
class function TArcFaceSDKIEVersion.ReadIEBitmap(
  ABitmap: TIEBitmap; // 源位图
  var AImgDataInfo: TImgdataInfo; // 图像数据结构
  AOutIEBitmp: TIEBitmap; // 缩放后输出的目标位图
  AResampleWidth: Integer = 0; // 缩放宽度
  AResampleHeight: Integer = 0; // 缩放高度
  AResampleFilter: TResampleFilter = rfNone; // 绽放滤镜
  AContrast: Double = 0 // 调整对比度
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
  Result := false;
  if ABitmap = nil then
    Exit;

  if (AResampleWidth <> 0) or (AResampleHeight <> 0) then
  begin
    lBitMap := TIEBitmap.Create;
    // 复制数据
    lBitMap.Assign(ABitmap);
    proc := TImageEnProc.Create(nil);
    try
      proc.AttachedIEBitmap := lBitMap;
      // 如果指定宽度和高度均大于0
      if (AResampleWidth > 0) and (AResampleHeight > 0) then
      begin
        // 如果宽比>高比则以宽为基准进行调整，否则以高为基准进行调整，相当于宽优先
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
      // 如果指定宽小于0，则检查当前宽是否小于指定宽的绝对值，
      // 如果小于的则以宽度为基准进行调整
      else if (AResampleWidth < 0) then
      begin
        if lBitMap.Width < Abs(AResampleWidth) then
          proc.Resample(Abs(AResampleWidth),
            Round(Abs(AResampleWidth) / lBitMap.Width *
            lBitMap.Height), AResampleFilter)
      end
      // 如果指定高小于0，则检查当前高是否小于指定高的绝对值，
      // 如果小于的则以高度为基准进行调整
      else if (AResampleHeight < 0) then
      begin
        if lBitMap.Height < Abs(AResampleHeight) then
          proc.Resample(Round(Abs(AResampleHeight) / lBitMap.Height *
            lBitMap.Width), Abs(AResampleHeight), AResampleFilter)
      end
      // 如果指定宽大于0，则以宽度为基准进行调整
      else if AResampleWidth > 0 then
        proc.Resample(AResampleWidth, Round(AResampleWidth / lBitMap.Width *
          lBitMap.Height), AResampleFilter)
        // 如果指定高大于0，则以高度为基准进行调整
      else if AResampleHeight > 0 then
        proc.Resample(Round(AResampleHeight / lBitMap.Height * lBitMap.Width),
          AResampleHeight, AResampleFilter);
      // 调整对比度
      if AContrast > 0 then
        proc.Contrast(AContrast);

    finally
      proc.Free;
    end;

  end
  else
    lBitMap := ABitmap;

  Result := false;
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

{ -------------------------------------------------------------------------------
  过程名:    TArcFaceSDKIEVersion.TrackFacesAndAgeGenderFromIEBitmap
  作者:      NJTZ
  日期:      2018.09.22
  功能:      从IEBitmap中获取人脸位置、性别和年龄信息列表（追踪模式）
  参数:
  ABitmap: TIEBitmap; // 源位图
  var AFaceInfos: TList<TFaceBaseInfo>; // 人脸基本信息
  AUseCache: Boolean // 是否使用缓存空间，使用可以避免频繁申请释放内存，应注意线程安全
  返回值:    Boolean
  ------------------------------------------------------------------------------- }
function TArcFaceSDKIEVersion.TrackFacesAndAgeGenderFromIEBitmap(
  ABitmap: TIEBitmap;
  var AFaceInfos: TList<TFaceBaseInfo>;
  AUseCache: Boolean
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
  Result := false;

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
    // 人脸检测
    R := AFT_FSDK_FaceFeatureDetect(FFaceTrackingEngine, @offInput, pFaceRes);
    if R = MOK then
    begin
      // 分解人脸位置信息
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

        // 检测年龄
        if (FFaceAgeEngine <> nil) then
        begin
          with lFaceRes_Age do
          begin
            pFaceRectArray := @ArrFaceRect[0];
            pFaceOrientArray := @ArrFaceOrient[0];
            lFaceNumber := iFaces;
          end;

          if ASAE_FSDK_AgeEstimation_Preview(
            FFaceAgeEngine, // [in] age estimation engine
            @offInput, // [in] the original image information
            // [in] the face rectangles information
            @lFaceRes_Age,
            // [out] the results of age estimation
            lAgeRes
            ) = MOK then
            // 分解人脸年龄
            ExtractFaceAges(lAgeRes, lAges);

        end;

        // 检测性别
        if (FFaceGenderEngine <> nil) then
        begin
          with lFaceRes_Gender do
          begin
            pFaceRectArray := @ArrFaceRect[0];
            pFaceOrientArray := @ArrFaceOrient[0];
            lFaceNumber := iFaces;
          end;

          if ASGE_FSDK_GenderEstimation_Preview(
            FFaceGenderEngine, // [in] Gender estimation engine
            @offInput, // [in] the original imGender information
            // [in] the face rectangles information
            @lFaceRes_Gender,
            // [out] the results of Gender estimation
            lGenderRes
            ) = MOK then
            // 分解人脸性别
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

{ -------------------------------------------------------------------------------
  过程名:    TArcFaceSDKIEVersion.DetectFacesAndAgeGenderFromIEBitmap
  作者:      NJTZ
  日期:      2018.09.22
  功能:      从IEBitmap中获取人脸位置、性别和年龄信息列表
  参数:
  ABitmap: TIEBitmap; // 源位图
  var AFaceInfos: TList<TFaceBaseInfo>; // 人脸基本信息列表
  AOutIEBitmp: TIEBitmap; // 缩放后输出的目标位图
  AResampleWidth, // 缩放宽度
  AResampleHeight: Integer; // 缩放高度
  AResampleFilter: TResampleFilter; // 缩放滤镜
  AContrast: Double; // 对比度，默认0不调整
  AUseCache: Boolean // 是否使用缓存空间，使用可以避免频繁申请释放内存，应注意线程安全
  返回值:    Boolean
  ------------------------------------------------------------------------------- }
function TArcFaceSDKIEVersion.DetectFacesAndAgeGenderFromIEBitmap(
  ABitmap: TIEBitmap;
  var AFaceInfos: TList<TFaceBaseInfo>;
  AOutIEBitmp: TIEBitmap;
  AResampleWidth,
  AResampleHeight: Integer;
  AResampleFilter: TResampleFilter;
  AContrast: Double;
  AUseCache: Boolean
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
  Result := false;

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
    // 人脸检测
    if AFD_FSDK_StillImageFaceDetection(FFaceDetectionEngine, @offInput,
      pFaceRes) = MOK then
    begin
      // 分解人脸位置信息
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

        // 检测年龄
        if (FFaceAgeEngine <> nil) then
        begin
          with lFaceRes_Age do
          begin
            pFaceRectArray := @ArrFaceRect[0];
            pFaceOrientArray := @ArrFaceOrient[0];
            lFaceNumber := iFaces;
          end;

          if ASAE_FSDK_AgeEstimation_StaticImage(
            FFaceAgeEngine, // [in]年龄评估引擎实例句柄
            @offInput, // [in]原始图像数据
            @lFaceRes_Age, // [in]人脸框信息
            lAgeRes // [out]年龄评估结果
            ) = MOK then
            // 分解人脸年龄
            ExtractFaceAges(lAgeRes, lAges);

        end;

        // 检测性别
        if (FFaceGenderEngine <> nil) then
        begin
          with lFaceRes_Gender do
          begin
            pFaceRectArray := @ArrFaceRect[0];
            pFaceOrientArray := @ArrFaceOrient[0];
            lFaceNumber := iFaces;
          end;

          if ASGE_FSDK_GenderEstimation_StaticImage(
            FFaceGenderEngine, // [in]性别评估引擎实例句柄
            @offInput, // [in]原始图像数据
            @lFaceRes_Gender, // [in]人脸框信息
            lGenderRes // [out]性别评估结果
            ) = MOK then
            // 分解人脸性别
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

{ -------------------------------------------------------------------------------
  过程名:    TArcFaceSDKIEVersion.DRAGfromIEBitmap
  作者:      NJTZ
  日期:      2018.09.22
  功能:      从TIEBitmap中获取人脸位置、性别和年龄信息列表,部分参数使用类实例属性值
  参数:
  ABitmap: TIEBitmap; // 源位图
  var AFaceInfos: TList<TFaceBaseInfo>; // 人脸基本信息列表
  var AFaceModels: TFaceModels; // 人脸特征集
  AOutIEBitmp: TIEBitmap; // 缩放后输出的目标位图
  AUseCache: Boolean // 是否使用缓存空间，使用可以避免频繁申请释放内存，应注意线程安全
  返回值:    Boolean
  ------------------------------------------------------------------------------- }
function TArcFaceSDKIEVersion.DRAGfromIEBitmap(
  ABitmap: TIEBitmap;
  var AFaceInfos: TList<TFaceBaseInfo>;
  var AFaceModels: TFaceModels;
  AOutIEBitmp: TIEBitmap;
  AUseCache: Boolean
  ): Boolean;
begin
  Result := DRAGfromIEBitmap(ABitmap, AFaceInfos, AFaceModels,
    AOutIEBitmp, FResampleWidth, FResampleHeight, FResampleFilter,
    FContrastRatio, AUseCache);
end;

{ -------------------------------------------------------------------------------
  过程名:    TArcFaceSDKIEVersion.DRAGfromFile
  作者:      NJTZ
  日期:      2018.09.22
  功能:      从图像文件中获取人脸位置、性别和年龄信息列表,部分参数使用类实例属性值
  参数:
  AFileName: string; // 图像文件名，支持 ImageEN 支持的所有图像格式
  var AFaceInfos: TList<TFaceBaseInfo>; // 人脸基本信息列表
  var AFaceModels: TFaceModels; // 人脸特征集
  AOutIEBitmp: TIEBitmap; // 缩放后输出的目标位图
  AUseCache: Boolean // 是否使用缓存空间，使用可以避免频繁申请释放内存，应注意线程安全
  返回值:    Boolean
  ------------------------------------------------------------------------------- }
function TArcFaceSDKIEVersion.DRAGfromFile(
  AFileName: string;
  var AFaceInfos: TList<TFaceBaseInfo>;
  var AFaceModels: TFaceModels;
  AOutIEBitmp: TIEBitmap;
  AUseCache: Boolean
  ): Boolean;
begin
  Result := DRAGfromFile(AFileName, AFaceInfos, AFaceModels,
    AOutIEBitmp, FResampleWidth, FResampleHeight, FResampleFilter,
    FContrastRatio, AUseCache);
end;

{ -------------------------------------------------------------------------------
  过程名:    TArcFaceSDKIEVersion.DRAGfromFile
  作者:      NJTZ
  日期:      2018.09.22
  功能:      从图像文件中获取人脸位置、年龄、性别和特征信息列表
  参数:
  AFileName: string; // 图像文件名，支持 ImageEN 支持的所有图像格式
  var AFaceInfos: TList<TFaceBaseInfo>; // 人脸基本信息列表
  var AFaceModels: TFaceModels; // 人脸特征集
  AOutIEBitmp: TIEBitmap; // 缩放后输出的目标位图
  AResampleWidth, // 缩放宽度
  AResampleHeight: Integer; // 缩放高度
  AResampleFilter: TResampleFilter; // 缩放滤镜
  AContrast: Double; // 对比度，默认0不调整
  AUseCache: Boolean // 是否使用缓存空间，使用可以避免频繁申请释放内存，应注意线程安全
  返回值:    Boolean
  ------------------------------------------------------------------------------- }
function TArcFaceSDKIEVersion.DRAGfromFile(
  AFileName: string;
  var AFaceInfos: TList<TFaceBaseInfo>;
  var AFaceModels: TFaceModels;
  AOutIEBitmp: TIEBitmap;
  AResampleWidth,
  AResampleHeight: Integer;
  AResampleFilter: TResampleFilter;
  AContrast: Double;
  AUseCache: Boolean
  ): Boolean;
var
  lIEBitmap: TIEBitmap;
  io: TImageEnIO;
begin
  Result := false;
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

{ -------------------------------------------------------------------------------
  过程名:    TArcFaceSDKIEVersion.RzFicFaceDataFeatureExtractionFromIEBitmap
  作者:      NJTZ
  日期:      2018.09.23
  功能:      从位图中提取人脸特征，不返回人脸位置，使用人证SDK引擎
  参数:      ABitmap: TIEBitmap; isVideo: Boolean; AOutIEBitmp: TIEBitmap; AResampleWidth, AResampleHeight: Integer; AResampleFilter: TResampleFilter; AContrast: Double; AUseCache: Boolean
  返回值:    Boolean
  ------------------------------------------------------------------------------- }
{$IFDEF ARC_RZ_SDK}


function TArcFaceSDKIEVersion.RzFicFaceDataFeatureExtractionFromIEBitmap(
  ABitmap: TIEBitmap; isVideo: Boolean; AOutIEBitmp: TIEBitmap;
  AResampleWidth, AResampleHeight: Integer; AResampleFilter: TResampleFilter;
  AContrast: Double; AUseCache: Boolean): Boolean;
var
  lFaceRes: AFIC_FSDK_FACERES;
begin
  Result := RzFicFaceDataFeatureExtractionFromIEBitmap(ABitmap, isVideo,
    lFaceRes, AOutIEBitmp, AResampleWidth,
    AResampleHeight, AResampleFilter, AContrast, AUseCache);
end;
{$ENDIF}

{ -------------------------------------------------------------------------------
  过程名:    TArcFaceSDKIEVersion.RzFicFaceDataFeatureExtractionFromIEBitmap
  作者:      NJTZ
  日期:      2018.09.23
  功能:      从位图中提取人脸特征，返回人脸位置（第一个），使用人证SDK引擎
  参数:      ABitmap: TIEBitmap; isVideo: Boolean; var AFaceRes: AFIC_FSDK_FACERES; AOutIEBitmp: TIEBitmap; AResampleWidth, AResampleHeight: Integer; AResampleFilter: TResampleFilter; AContrast: Double; AUseCache: Boolean
  返回值:    Boolean
  ------------------------------------------------------------------------------- }
{$IFDEF ARC_RZ_SDK}


function TArcFaceSDKIEVersion.RzFicFaceDataFeatureExtractionFromIEBitmap(
  ABitmap: TIEBitmap; isVideo: Boolean; var AFaceRes: AFIC_FSDK_FACERES;
  AOutIEBitmp: TIEBitmap; AResampleWidth, AResampleHeight: Integer;
  AResampleFilter: TResampleFilter; AContrast: Double; AUseCache: Boolean):
  Boolean;
var
  lImgData: TImgdataInfo;
  RyZPInput: ASVLOFFSCREEN;
  nRet: MRESULT;
{$IFDEF DEBUG}
  T: Cardinal;
{$ENDIF}
begin
  Result := false;

  if FFaceRzFicEngine = nil then
    Exit;

{$IFDEF DEBUG}
  T := GetTickCount;
{$ENDIF}
  if AUseCache then
    lImgData := FImgDataInfoCache
  else
    lImgData.pImgData := nil;

  if not ReadIEBitmap(ABitmap, lImgData, AOutIEBitmp, AResampleWidth,
    AResampleHeight, AResampleFilter, AContrast) then
    Exit;

{$IFDEF DEBUG}
  T := GetTickCount - T;
  DoLog('载入数据耗时：' + IntToStr(T));
{$ENDIF}
  RyZPInput.u32PixelArrayFormat := ASVL_PAF_RGB24_B8G8R8;
  FillChar(RyZPInput.pi32Pitch, SizeOf(RyZPInput.pi32Pitch), 0);
  FillChar(RyZPInput.ppu8Plane, SizeOf(RyZPInput.ppu8Plane), 0);

  RyZPInput.i32Width := lImgData.Width;
  RyZPInput.i32Height := lImgData.Height;

  RyZPInput.ppu8Plane[0] := IntPtr(lImgData.pImgData);
  RyZPInput.pi32Pitch[0] := lImgData.LineBytes;

  // 人脸检测
{$IFDEF DEBUG}
  T := GetTickCount;
{$ENDIF}
  nRet := ArcSoft_FIC_FaceDataFeatureExtraction(
    FFaceRzFicEngine, // [in]  FIC 引擎Handle
    Bool2Int(isVideo), // [in]  人脸数据类型 1-视频 0-静态图片
    @RyZPInput, // [in]  人脸图像原始数据
    // pFaceRes: LPAFIC_FSDK_FACERES
    AFaceRes // [out] 人脸属性 人脸数/人脸框/角度
    );
  Result := nRet = MOK;

{$IFDEF DEBUG}
  T := GetTickCount - T;
  DoLog('检测人脸耗时：' + IntToStr(T));
{$ENDIF}
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
{$ENDIF}

{ -------------------------------------------------------------------------------
  过程名:    TArcFaceSDKIEVersion.RzFicFaceDataFeatureExtractionFromFile
  作者:      NJTZ
  日期:      2018.09.23
  功能:      从图像文件中提取人脸特征，不返回人脸位置，使用人证SDK引擎
  参数:      AFile: String; isVideo: Boolean; AOutIEBitmp: TIEBitmap; AResampleWidth, AResampleHeight: Integer; AResampleFilter: TResampleFilter; AContrast: Double; AUseCache: Boolean
  返回值:    Boolean
  ------------------------------------------------------------------------------- }
{$IFDEF ARC_RZ_SDK}


function TArcFaceSDKIEVersion.RzFicFaceDataFeatureExtractionFromFile(AFile:
  String; isVideo: Boolean; AOutIEBitmp: TIEBitmap; AResampleWidth,
  AResampleHeight: Integer; AResampleFilter: TResampleFilter; AContrast:
  Double; AUseCache: Boolean): Boolean;
var
  lFaceRes: AFIC_FSDK_FACERES;
begin
  Result := RzFicFaceDataFeatureExtractionFromFile(AFile, isVideo, lFaceRes,
    AOutIEBitmp, AResampleWidth,
    AResampleHeight, AResampleFilter, AContrast, AUseCache);
end;
{$ENDIF}
{ -------------------------------------------------------------------------------
  过程名:    TArcFaceSDKIEVersion.RzFicFaceDataFeatureExtractionFromFile
  作者:      NJTZ
  日期:      2018.09.23
  功能:      从图像文件中提取人脸特征，返回人脸位置，使用人证SDK引擎
  参数:      AFile: String; isVideo: Boolean; var AFaceRes: AFIC_FSDK_FACERES; AOutIEBitmp: TIEBitmap; AResampleWidth, AResampleHeight: Integer; AResampleFilter: TResampleFilter; AContrast: Double; AUseCache: Boolean
  返回值:    Boolean
  ------------------------------------------------------------------------------- }
{$IFDEF ARC_RZ_SDK}


function TArcFaceSDKIEVersion.RzFicFaceDataFeatureExtractionFromFile(AFile:
  String; isVideo: Boolean; var AFaceRes: AFIC_FSDK_FACERES; AOutIEBitmp:
  TIEBitmap; AResampleWidth, AResampleHeight: Integer; AResampleFilter:
  TResampleFilter; AContrast: Double; AUseCache: Boolean): Boolean;
var
  lImgData: TImgdataInfo;
  offInput: ASVLOFFSCREEN;
  nRet: MRESULT;
{$IFDEF DEBUG}
  T: Cardinal;
{$ENDIF}
begin
  Result := false;

  if FFaceRzFicEngine = nil then
    Exit;

{$IFDEF DEBUG}
  T := GetTickCount;
{$ENDIF}
  if AUseCache then
    lImgData := FImgDataInfoCache
  else
    lImgData.pImgData := nil;

  if not ReadFromFile(AFile, lImgData, AOutIEBitmp, AResampleWidth,
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

  // 人脸检测
{$IFDEF DEBUG}
  T := GetTickCount;
{$ENDIF}
  nRet := ArcSoft_FIC_FaceDataFeatureExtraction(
    FFaceRzFicEngine, // [in]  FIC 引擎Handle
    Bool2Int(isVideo), // [in]  人脸数据类型 1-视频 0-静态图片
    @offInput, // [in]  人脸图像原始数据
    // pFaceRes: LPAFIC_FSDK_FACERES
    AFaceRes // [out] 人脸属性 人脸数/人脸框/角度
    );
  Result := nRet = MOK;

{$IFDEF DEBUG}
  T := GetTickCount - T;
  DoLog('检测人脸耗时：' + IntToStr(T));
{$ENDIF}
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
{$ENDIF}
{ -------------------------------------------------------------------------------
  过程名:    TArcFaceSDKIEVersion.RzFicCompareFromIEBitmap
  作者:      NJTZ
  日期:      2018.09.23
  功能:      根据人脸抓拍照和二代证照片完成一个完整的人证比对过程，使用人证SDK引擎
  参数:      AZjz, ARyZP: TIEBitmap; isVideo: Boolean; var ASimilarScore: Single; var ACompareResult: Integer; AThreshold: Single = 0.82
  返回值:    Boolean
  ------------------------------------------------------------------------------- }
{$IFDEF ARC_RZ_SDK}


function TArcFaceSDKIEVersion.RzFicCompareFromIEBitmap(AZjz, ARyZP: TIEBitmap;
  isVideo: Boolean; var ASimilarScore: Single; var ACompareResult: Integer;
  AThreshold: Single = 0.82): Boolean;
var
  lFaceRes: AFIC_FSDK_FACERES;
begin
  Result := RzFicCompareFromIEBitmap(AZjz, ARyZP, isVideo, ASimilarScore,
    ACompareResult,
    lFaceRes, AThreshold);
end;
{$ENDIF}
{ -------------------------------------------------------------------------------
  过程名:    TArcFaceSDKIEVersion.RzFicCompareFromIEBitmap
  作者:      NJTZ
  日期:      2018.09.23
  功能:      根据人脸抓拍照和二代证照片完成一个完整的人证比对过程，使用人证SDK引擎
  参数:      AZjz, ARyZP: TIEBitmap; isVideo: Boolean; var ASimilarScore: Single; var ACompareResult: Integer; var AFaceRes: AFIC_FSDK_FACERES; AThreshold: Single = 0.82
  返回值:    Boolean
  ------------------------------------------------------------------------------- }
{$IFDEF ARC_RZ_SDK}


function TArcFaceSDKIEVersion.RzFicCompareFromIEBitmap(AZjz, ARyZP: TIEBitmap;
  isVideo: Boolean; var ASimilarScore: Single; var ACompareResult: Integer;
  var AFaceRes: AFIC_FSDK_FACERES; AThreshold: Single = 0.82): Boolean;
begin
  Result := false;
  if RzFicIdCardDataFeatureExtractionFromIEBitmap(AZjz, nil, 0, 0,
    TResampleFilter.rfNone, 0) then
    if RzFicFaceDataFeatureExtractionFromIEBitmap(ARyZP, isVideo, AFaceRes,
      nil, 0, 0, TResampleFilter.rfNone, 0, false) then
      Result := RzFicFaceIdCardCompare(ASimilarScore, ACompareResult,
        AThreshold);
end;
{$ENDIF}
{ -------------------------------------------------------------------------------
  过程名:    TArcFaceSDKIEVersion.RzFicCompareFromFile
  作者:      NJTZ
  日期:      2018.09.23
  功能:      根据人脸抓拍照和二代证照片完成一个完整的人证比对过程，使用人证SDK引擎
  参数:      AZjz, ARyZP: string; isVideo: Boolean; var ASimilarScore: Single; var ACompareResult: Integer; AThreshold: Single = 0.82
  返回值:    Boolean
  ------------------------------------------------------------------------------- }
{$IFDEF ARC_RZ_SDK}


function TArcFaceSDKIEVersion.RzFicCompareFromFile(AZjz, ARyZP: string;
  isVideo: Boolean; var ASimilarScore: Single; var ACompareResult: Integer;
  AThreshold: Single = 0.82): Boolean;
var
  lFaceRes: AFIC_FSDK_FACERES;
begin
  Result := RzFicCompareFromFile(AZjz, ARyZP, isVideo, ASimilarScore,
    ACompareResult, lFaceRes, AThreshold);
end;
{$ENDIF}

{ -------------------------------------------------------------------------------
  过程名:    TArcFaceSDKIEVersion.RzFicCompareFromFile
  作者:      NJTZ
  日期:      2018.09.23
  功能:      根据人脸抓拍照和二代证照片完成一个完整的人证比对过程，使用人证SDK引擎
  参数:      AZjz, ARyZP: string; isVideo: Boolean; var ASimilarScore: Single; var ACompareResult: Integer; var AFaceRes: AFIC_FSDK_FACERES; AThreshold: Single = 0.82
  返回值:    Boolean
  ------------------------------------------------------------------------------- }
{$IFDEF ARC_RZ_SDK}


function TArcFaceSDKIEVersion.RzFicCompareFromFile(AZjz, ARyZP: string;
  isVideo: Boolean; var ASimilarScore: Single; var ACompareResult: Integer;
  var AFaceRes: AFIC_FSDK_FACERES; AThreshold: Single = 0.82): Boolean;
begin
  Result := false;
  if RzFicIdCardDataFeatureExtractionFromFile(AZjz, nil, 0, 0,
    TResampleFilter.rfNone, 0) then
    if RzFicFaceDataFeatureExtractionFromFile(ARyZP, isVideo, AFaceRes, nil,
      0, 0, TResampleFilter.rfNone, 0, false) then
      Result := RzFicFaceIdCardCompare(ASimilarScore, ACompareResult,
        AThreshold);
end;
{$ENDIF}
{ -------------------------------------------------------------------------------
  过程名:    TArcFaceSDKIEVersion.RzFicIdCardDataFeatureExtractionFromIEBitmap
  作者:      NJTZ
  日期:      2018.09.23
  功能:      从二代证照片位图中提取人脸特征，使用人证SDK引擎
  参数:      ABitmap, AOutIEBitmp: TIEBitmap; AResampleWidth, AResampleHeight: Integer; AResampleFilter: TResampleFilter; AContrast: Double
  返回值:    Boolean
  ------------------------------------------------------------------------------- }
{$IFDEF ARC_RZ_SDK}


function TArcFaceSDKIEVersion.RzFicIdCardDataFeatureExtractionFromIEBitmap(
  ABitmap, AOutIEBitmp: TIEBitmap; AResampleWidth, AResampleHeight: Integer;
  AResampleFilter: TResampleFilter; AContrast: Double): Boolean;
var
  lImgData: TImgdataInfo;
  offInput: ASVLOFFSCREEN;
  nRet: MRESULT;
{$IFDEF DEBUG}
  T: Cardinal;
{$ENDIF}
begin
  Result := false;
  if FFaceRzFicEngine = nil then
    Exit;

{$IFDEF DEBUG}
  T := GetTickCount;
{$ENDIF}
  lImgData.pImgData := nil;

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

  // 证件照检测
  nRet := ArcSoft_FIC_IdCardDataFeatureExtraction(
    FFaceRzFicEngine, // [in]  FIC 引擎Handle
    @offInput // [in]  图像原始数据
    );

  Result := nRet = MOK;

  if lImgData.pImgData <> nil then
    FreeMem(lImgData.pImgData);
end;
{$ENDIF}
{ -------------------------------------------------------------------------------
  过程名:    TArcFaceSDKIEVersion.RzFicIdCardDataFeatureExtractionFromFile
  作者:      NJTZ
  日期:      2018.09.23
  功能:      从二代证照片文件中提取人脸特征，使用人证SDK引擎
  参数:      AFile: String; AOutIEBitmp: TIEBitmap; AResampleWidth, AResampleHeight: Integer; AResampleFilter: TResampleFilter; AContrast: Double
  返回值:    Boolean
  ------------------------------------------------------------------------------- }
{$IFDEF ARC_RZ_SDK}


function TArcFaceSDKIEVersion.RzFicIdCardDataFeatureExtractionFromFile(AFile:
  String; AOutIEBitmp: TIEBitmap; AResampleWidth, AResampleHeight: Integer;
  AResampleFilter: TResampleFilter; AContrast: Double): Boolean;
var
  lImgData: TImgdataInfo;
  offInput: ASVLOFFSCREEN;
  nRet: MRESULT;
{$IFDEF DEBUG}
  T: Cardinal;
{$ENDIF}
begin
  Result := false;
  if FFaceRzFicEngine = nil then
    Exit;

{$IFDEF DEBUG}
  T := GetTickCount;
{$ENDIF}
  lImgData.pImgData := nil;

  if not ReadFromFile(AFile, lImgData, AOutIEBitmp, AResampleWidth,
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

  // 证件照检测
  nRet := ArcSoft_FIC_IdCardDataFeatureExtraction(
    FFaceRzFicEngine, // [in]  FIC 引擎Handle
    @offInput // [in]  图像原始数据
    );

  Result := nRet = MOK;

  if lImgData.pImgData <> nil then
    FreeMem(lImgData.pImgData);
end;
{$ENDIF}

end.
