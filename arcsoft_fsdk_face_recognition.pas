(*******************************************************************************
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
 *********************************************************************************)

unit arcsoft_fsdk_face_recognition;

interface
uses
  Windows, Messages, SysUtils, Classes, amcomdef, asvloffscreendef;

const
  ArcRecognitionDll = 'libarcsoft_fsdk_face_n_recognition.dll';

type
  AFR_FSDK_OrientCode = MInt32;
{$EXTERNALSYM AFR_FSDK_OrientCode}

  (*******************************************************************************************
   FaceRecognition Face Orientation
   *******************************************************************************************)
Type
  TAFR_FSDK_OrientCode =
    (AFR_FSDK_FOC_0 = $1, //0 degree
    AFR_FSDK_FOC_90 = $2, //90 degree
    AFR_FSDK_FOC_270 = $3, //270 degree
    AFR_FSDK_FOC_180 = $4, //180 degree
    AFR_FSDK_FOC_30 = $5, //30 degree
    AFR_FSDK_FOC_60 = $6, //60 degree
    AFR_FSDK_FOC_120 = $7, //120 degree
    AFR_FSDK_FOC_150 = $8, //150 degree
    AFR_FSDK_FOC_210 = $9, //210 degree
    AFR_FSDK_FOC_240 = $A, //240 degree
    AFR_FSDK_FOC_300 = $B, //300 degree
    AFR_FSDK_FOC_330 = $C//330 degree,
    );

  //type
  //_AFR_FSDK_OrientCode = AFR_FSDK_FOC_0 .. AFR_FSDK_FOC_330;
  {.$EXTERNALSYM _AFR_FSDK_OrientCode}

  (*******************************************************************************************
   FaceRecognition Face Information
   *******************************************************************************************)
  AFR_FSDK_FACEINPUT = record
    rcFace: MRECT; //The bounding box of face
    lOrient: AFR_FSDK_OrientCode; //The orientation of face
  end;

  LPAFR_FSDK_FACEINPUT = ^AFR_FSDK_FACEINPUT;

  (*******************************************************************************************
   FaceRecognition Feature Information
   *******************************************************************************************)
  AFR_FSDK_FACEMODEL = record
    pbFeature: PByte; //The extracted features
    lFeatureSize: MInt32; //The size of pbFeature
  end;

  LPAFR_FSDK_FACEMODEL = ^AFR_FSDK_FACEMODEL;

  (*******************************************************************************************
   FaceRecognition Version Information
   *******************************************************************************************)
  AFR_FSDK_Version = record
    lCodebase: MInt32; //Codebase version number
    lMajor: MInt32; //Major version number
    lMinor: MInt32; //Minor version number
    lBuild: MInt32; //Build version number, increasable only
    lFeatureLevel: MInt32; //Feature level, used for judging update or not
    Version: MPChar; //Version in string form
    BuildDate: MPChar; //Latest build Date
    CopyRight: MPChar; //Copyright
  end;

  LPAFR_FSDK_Version = ^AFR_FSDK_Version;

  (************************************************************************
   * Description:
   *	 The aFunction will be used to Initialize the face recognition engine.
   * Return value:
   *	 Return MOK if success, otherwise fail
   ************************************************************************)
function AFR_FSDK_InitialEngine(
  AppId: MPChar; //[in]  APPID
  SDKKey: MPChar; //[in]  SDKKEY
  pMem: PByte; //[in]	 User allocated memory for the engine
  lMemSize: MInt32; //[in]	 User allocated memory size
  //[out] Pointing to the face recognition engine:
  var hEngine: MHandle): MRESULT; Cdecl; external ArcRecognitionDll;

(************************************************************************
 * The aFunction used to get one face's feature.
 *
 * Comment:
 *  The pFaceRes should be the results of face tracking or face detection
 *  The aResult will be loaded to pFaceModels.
 *
 ************************************************************************)
function AFR_FSDK_ExtractFRFeature(
  hEngine: MHandle; //[in]  The face recognition engine
  pInputImage: LPASVLOFFSCREEN; //[in]  The input face images used to enroll
  pFaceRes: LPAFR_FSDK_FACEINPUT; //[in]  The faces'position and orientation
  //[out] The face feature information:
  var FaceModels: AFR_FSDK_FACEMODEL): MRESULT; Cdecl;
  external ArcRecognitionDll;

(************************************************************************
 * The aFunction used to do face authentication, i.e. comparing two face if they are same person.
 ************************************************************************)
function AFR_FSDK_FacePairMatching(
  hEngine: MHandle; //[in]  The face recognition engine
  reffeature: LPAFR_FSDK_FACEMODEL; //[in]  The reference face feature
  probefeature: LPAFR_FSDK_FACEMODEL; //[in]  The probing face feature.
  var fSimilScore: MFloat//[out] The authentication result.
  ): MRESULT; Cdecl; external ArcRecognitionDll;

(************************************************************************
 * The aFunction used to uninitialize the face recognition engine.
 ************************************************************************)
function AFR_FSDK_UninitialEngine(
  hEngine: MHandle//[in] The face recognition engine
  ): MRESULT; Cdecl; external ArcRecognitionDll;

function AFR_FSDK_GetVersion(hEngine: MHandle): LPAFR_FSDK_Version; Cdecl;
  external ArcRecognitionDll;

implementation

end.
