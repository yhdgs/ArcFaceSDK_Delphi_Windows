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

unit arcsoft_fsdk_face_tracking;

interface

uses
  Windows, Messages, SysUtils, Classes, amcomdef, asvloffscreendef;

const
  ArcTrackingDll = 'libarcsoft_fsdk_face_tracking.dll';

type
  AFT_FSDK_OrientPriority = MInt32;
{$EXTERNALSYM AFT_FSDK_OrientPriority}
  AFT_FSDK_OrientCode = MInt32;
{$EXTERNALSYM AFT_FSDK_OrientCode}

  (* ******************************************************************************************
    FaceTracking Orientation Priority
    ****************************************************************************************** *)
const
  AFT_FSDK_OPF_0_ONLY = $1; //0; 0; ...
  AFT_FSDK_OPF_90_ONLY = $2; //90; 90; ...
  AFT_FSDK_OPF_270_ONLY = $3; //270; 270; ...
  AFT_FSDK_OPF_180_ONLY = $4; //180; 180; ...
  AFT_FSDK_OPF_0_HIGHER_EXT = $5; //0; 90; 270; 180; 0; 90; 270; 180; ...

  //type
  //_AFT_FSDK_OrientPriority = AFT_FSDK_OPF_0_ONLY .. AFT_FSDK_OPF_0_HIGHER_EXT;
  //{$EXTERNALSYM _AFT_FSDK_OrientPriority}

  (* ******************************************************************************************
    FaceTracking Face Orientation
    ****************************************************************************************** *)
  AFT_FSDK_FOC_0 = $1; //0 degree
  AFT_FSDK_FOC_90 = $2; //90 degree
  AFT_FSDK_FOC_270 = $3; //270 degree
  AFT_FSDK_FOC_180 = $4; //180 degree;

  //type
  //_AFT_FSDK_OrientCode = AFT_FSDK_FOC_0 .. AFT_FSDK_FOC_180;
  //{$EXTERNALSYM _AFT_FSDK_OrientCode}
  (* ******************************************************************************************
    FaceTracking Face Information
    ****************************************************************************************** *)
type
  AFT_FSDK_FACERES = record
    nFace: MInt32; //number of faces detected
    lfaceOrient:
      IntPtr; //AFT_FSDK_OrientCode; //the face angle
    rcFace:
      IntPtr; //MRECT *; //The bounding box of face
  end;

  LPAFT_FSDK_FACERES = ^AFT_FSDK_FACERES;

  (* ******************************************************************************************
    FaceTracking Version Information
    ****************************************************************************************** *)
  AFT_FSDK_Version = record
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

  LPAFT_FSDK_Version = ^AFT_FSDK_Version;

  (* ***********************************************************************
    * The aFunction used to Initialize the face tracking engine.
    *********************************************************************** *)
function AFT_FSDK_InitialFaceEngine(
  AppId: MPChar; //[in]  APPID
  SDKKey: MPChar; //[in]  SDKKEY
  pMem: PByte; //[in]	User allocated memory for the engine
  lMemSize: MInt32; //[in]	User allocated memory size
  var hEngine: MHandle; //[out] Pointing to the tracking engine
  //[in]  Defining the priority of face orientation.
  iOrientPriority: AFT_FSDK_OrientPriority;
  nScale: MInt32; //[in]  An integer defining the minimal face to detect
  nMaxFaceNum: MInt32 //[in]  An integer defining the number of max faces
  ): MRESULT; Cdecl; external ArcTrackingDll;

(* ***********************************************************************
  * The aFunction used to detect and track face automatically.
  * Comment:
  *	The incoming image is scanned for faces.The aResult pFaceRes will be
  *  passed to this interface in the next calling round.
  *********************************************************************** *)
function AFT_FSDK_FaceFeatureDetect(
  hEngine: MHandle; //[in]  The face tracking engine
  pImgData: LPASVLOFFSCREEN; //[in]  The original image data
  var FaceRes: LPAFT_FSDK_FACERES //[out] The tracking result
  ): MRESULT; Cdecl; external ArcTrackingDll;

(* ***********************************************************************
  * The aFunction used to Uninitialize the tracking module.
  *********************************************************************** *)
function AFT_FSDK_UninitialFaceEngine(
  hEngine: MHandle //[in] The face tracking engine
  ): MRESULT; Cdecl; external ArcTrackingDll;

(* ***********************************************************************
  * The aFunction used to get version information of face tracking aLibrary.
  *********************************************************************** *)
function AFT_FSDK_GetVersion(hEngine: MHandle): LPAFT_FSDK_Version; Cdecl;
  external ArcTrackingDll;

implementation

end.
