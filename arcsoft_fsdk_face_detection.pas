(* ******************************************************************************
  * Copyright(c) ArcSoft, All right reserved.
  *
  * This aFile is ArcSoft's property. It contains ArcSoft's trade secret, proprietary
  * and confidential information.
  *
  * DO NOT DISTRIBUTE, DO NOT DUPLICATE OR TRANSMIT  ANY FORM WITHOUT PROPER
  * AUTHORIZATION.
  *
  * If you are not an intended recipient of this aFile, you must not copy,
  * distribute, modify, or take any action in reliance on it.
  *
  * If you have received this aFile in error, please immediately notify ArcSoft and
  * permanently delete the original and any copy of any aFile and any printout
  * thereof.
  * *******************************************************************************
  * C to Pascal by NJTZ 2017.10.10 eMail:yhdgs@qq.com
  ******************************************************************************** *)

unit arcsoft_fsdk_face_detection;

interface

uses
  Windows, Messages, SysUtils, Classes, amcomdef, asvloffscreendef;

const
  ArcDetectionDll = 'libarcsoft_fsdk_face_detection.dll';

type
  AFD_FSDK_OrientPriority = MInt32;
{$EXTERNALSYM AFD_FSDK_OrientPriority}
  AFD_FSDK_OrientCode = MInt32;
  LPAFD_FSDK_OrientCode = ^AFD_FSDK_OrientCode;
{$EXTERNALSYM AFD_FSDK_OrientCode}
  (* ******************************************************************************************
    FaceDetection Orientation Priority
    ****************************************************************************************** *)

  TAFD_FSDK_OrientPriority = (
    AFD_FSDK_OPF_0_ONLY = $1, //0; 0; ...
    AFD_FSDK_OPF_90_ONLY = $2, //90; 90; ...
    AFD_FSDK_OPF_270_ONLY = $3, //270; 270; ...
    AFD_FSDK_OPF_180_ONLY = $4, //180; 180; ...
    AFD_FSDK_OPF_0_HIGHER_EXT = $5 //0; 90; 270; 180; 0; 90; 270; 180; ...
    );
  { .$EXTERNALSYM _AFD_FSDK_OrientPriority }
  (* ******************************************************************************************
    FaceDetection Face Orientation
    ****************************************************************************************** *)

  TAFD_FSDK_OrientCode = (
    AFD_FSDK_FOC_0 = $1, //0 degree
    AFD_FSDK_FOC_90 = $2, //90 degree
    AFD_FSDK_FOC_270 = $3, //270 degree
    AFD_FSDK_FOC_180 = $4, //180 degree
    AFD_FSDK_FOC_30 = $5, //30 degree
    AFD_FSDK_FOC_60 = $6, //60 degree
    AFD_FSDK_FOC_120 = $7, //120 degree
    AFD_FSDK_FOC_150 = $8, //150 degree
    AFD_FSDK_FOC_210 = $9, //210 degree
    AFD_FSDK_FOC_240 = $A, //240 degree
    AFD_FSDK_FOC_300 = $B, //300 degree
    AFD_FSDK_FOC_330 = $C //330 degree
    );

  { .$EXTERNALSYM _AFD_FSDK_OrientCode }

  { *******************************************************************************************
    FaceDetection Face Information
    ******************************************************************************************* }
  AFD_FSDK_FACERES = record
    nFace: MInt32; //number of faces detected
    rcFace: IntPtr; //PMRECT; //The bounding box of face
    lfaceOrient: IntPtr; //AFD_FSDK_OrientCode; //the angle of each face
  end;

  LPAFD_FSDK_FACERES = ^AFD_FSDK_FACERES;

  { ******************************************************************************************
    FaceDetection Version Information
    ******************************************************************************************* }
  AFD_FSDK_Version = record
    lCodebase: MInt32; //Codebase version number
    lMajor:
      MInt32; //Major version number
    lMinor:
      MInt32; //Minor version number
    lBuild:
      MInt32; //Build version number, increasable only
    Version:
      MPChar; //Version in string form
    BuildDate:
      MPChar; //Latest build Date
    CopyRight:
      MPChar; //Copyright
  end;

  LPAFD_FSDK_Version = ^AFD_FSDK_Version;

  (* ***********************************************************************
    * The aFunction used to Initialize the face detection engine.
    *********************************************************************** *)
function AFD_FSDK_InitialFaceEngine(
  AppId: MPChar; //[in]  APPID
  SDKKey: MPChar; //[in]  SDKKEY
  pMem: PByte; //[in]	 User allocated memory for the engine
  lMemSize: MInt32; //[in]	 User allocated memory size
  var hEngine: MHandle; //[out] Pointing to the detection engine.
  //[in]  Defining the priority of face orientation
  iOrientPriority: AFD_FSDK_OrientPriority;
  //[in]  An integer defining the minimal face to detect relative to the maximum of image width and height.
  nScale: MInt32;
  //[in]  An integer defining the number of max faces to detection
  nMaxFaceNum: MInt32
  ): MRESULT; Cdecl; external ArcDetectionDll;

(* ***********************************************************************
  * The aFunction used to detect face in the input image.
  *********************************************************************** *)

function AFD_FSDK_StillImageFaceDetection(
  hEngine: MHandle; //[in]  The face detection engine
  pImgData: LPASVLOFFSCREEN; //[in]  The original image data
  var pFaceRes: //AFD_FSDK_FACERES
  LPAFD_FSDK_FACERES //[out] The detection result
  ): MRESULT; Cdecl; external ArcDetectionDll;

(* ***********************************************************************
  * The aFunction used to Uninitialize the detection module.
  *********************************************************************** *)
function AFD_FSDK_UninitialFaceEngine(
  hEngine: MHandle //[in]  The face detection engine
  ): MRESULT; Cdecl; external ArcDetectionDll;

(* ***********************************************************************
  * The aFunction used to get version information of face detection aLibrary.
  *********************************************************************** *)
function AFD_FSDK_GetVersion(hEngine: MHandle): LPAFD_FSDK_Version; Cdecl;
  external ArcDetectionDll;

implementation

end.
