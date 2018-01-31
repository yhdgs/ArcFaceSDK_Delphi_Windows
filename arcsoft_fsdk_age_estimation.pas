(*******************************************************************************
 Copyright(c) ArcSoft, All right reserved.

 This aFile is ArcSoft's property. It contains ArcSoft's trade secret, proprietary
 and confidential information.

 DO NOT DISTRIBUTE, DO NOT DUPLICATE OR TRANSMIT  ANY FORM WITHOUT PROPER
 AUTHORIZATION.

 If you are not an intended recipient of this aFile, you must not copy,
 distribute, modify, or take any action in reliance on it.

 If you have received this aFile in error, please immediately notify ArcSoft and
 permanently delete the original and any copy of any aFile and any printout
 thereof.
 *******************************************************************************)

unit arcsoft_fsdk_age_estimation;

interface

uses
  Windows, Messages, SysUtils, Classes, amcomdef, asvloffscreendef;

const

  ArcAgeEstimationDll = 'libarcsoft_fsdk_age_estimation.dll';


  //This enumeration defines the orientation of the face in anti-clockwise sequence.

type
  ASAE_FSDK_AgeOrientCode =
    (ASAE_FSDK_FOC_Age_0 = $1, //0 degree
    ASAE_FSDK_FOC_Age_90 = $2, //90 degree
    ASAE_FSDK_FOC_Age_270 = $3, //270 degree
    ASAE_FSDK_FOC_Age_180 = $4, //180 degree
    ASAE_FSDK_FOC_Age_30 = $5, //30 degree
    ASAE_FSDK_FOC_Age_60 = $6, //60 degree
    ASAE_FSDK_FOC_Age_120 = $7, //120 degree
    ASAE_FSDK_FOC_Age_150 = $8, //150 degree
    ASAE_FSDK_FOC_Age_210 = $9, //210 degree
    ASAE_FSDK_FOC_Age_240 = $A, //240 degree
    ASAE_FSDK_FOC_Age_300 = $B, //300 degree
    ASAE_FSDK_FOC_Age_330 = $C//330 degree,
    );
  //type
  //ASAE_FSDK_AgeOrientCode = ASAE_FSDK_FOC_Age_0..ASAE_FSDK_FOC_Age_330;
{$EXTERNALSYM ASAE_FSDK_AgeOrientCode}

  //This structure defines the input face information.
  ASAE_FSDK_AGEFACEINPUT = record
    //bounding boxes of input faces.
    //MRECT * pFaceRectArray;
    pFaceRectArray: PMRECT;
    //orientations of input faces. It must be set as one item of "ASAE_FSDK_AgeOrientCode".
    //MInt32 * pFaceOrientArray;
    pFaceOrientArray: PINT;
    //number of input faces, and it must be greater than or euqual to 0.
    lFaceNumber: MInt32;
  end;

  LPASAE_FSDK_AGEFACEINPUT = ^ASAE_FSDK_AGEFACEINPUT;

  //This structure defines the age estimation results.
  ASAE_FSDK_AGERESULT = record
    //the age aResult aArray with length of "lFaceNumber"
    //the value will be equal to or greater than 0.
    //specifically, "0" represents unknown, and the value greater than 0 represents valid age.
    //MInt32 * pAgeResultArray;
    pAgeResultArray: IntPtr;
    //It is the same as "lFaceNumber" in "ASAE_FSDK_AGEFACEINPUT".
    lFaceNumber: MInt32;
  end;

  LPASAE_FSDK_AGERESULT = ^ASAE_FSDK_AGERESULT;

  //This structure describes the version information of the age estimation library.
  ASAE_FSDK_Version = record
    lCodebase: MInt32; //code base version number
    lMajor: MInt32; //major version number
    lMinor: MInt32; //minor version number
    lBuild: MInt32; //build version number
    Version: MPChar; //version in string form
    BuildDate: MPChar; //latest build date
    CopyRight: MPChar; //copyright information
  end;

  TASAE_FSDK_Version = ASAE_FSDK_Version;
{$EXTERNALSYM TASAE_FSDK_Version}
  // *********************************************************************
  //This function is used to initialize the age estimation engine
  // ********************************************************************
function ASAE_FSDK_InitAgeEngine(
  AppId: MPChar; //[in]  APPID
  SDKKey: MPChar; //[in]  SDKKEY
  pMem: PByte; //[in]	 User allocated memory for the engine
  lMemSize: MInt32; //[in]	 allocated memory size
  var hEngine: MHandle//[out] pointer to the engine
  ): MRESULT; Cdecl; external ArcAgeEstimationDll;

// **************************************************************************
//This function is used to estimate age in static image mode automatically.
// **************************************************************************
function ASAE_FSDK_AgeEstimation_StaticImage(
  hEngine: MHandle; //[in] age estimation engine
  pImgInfo: LPASVLOFFSCREEN; //[in] the original image information
  pFaceRes: LPASAE_FSDK_AGEFACEINPUT; //[in] the face rectangles information.
  var pAgeRes: ASAE_FSDK_AGERESULT//[out] the results of age estimation
  ): MRESULT; Cdecl; external ArcAgeEstimationDll;

// *********************************************************************
//This function is used to estimate age in preview mode automatically.
// *********************************************************************
function ASAE_FSDK_AgeEstimation_Preview(
  hEngine: MHandle; //[in] age estimation engine
  pImgInfo: LPASVLOFFSCREEN; //[in] the original image information
  pFaceRes: LPASAE_FSDK_AGEFACEINPUT; //[in] the face rectangles information
  var pAgeRes: ASAE_FSDK_AGERESULT//[out] the results of age estimation
  ): MRESULT; Cdecl; external ArcAgeEstimationDll;

// ***********************************************************************
//This function is used to release the age estimation engine.
// ***********************************************************************
function ASAE_FSDK_UninitAgeEngine(
  hEngine: MHandle//[in]  pointer to the engine
  ): MRESULT; Cdecl; external ArcAgeEstimationDll;

// *********************************************************************
//This function is used to get the version information of the library.
// *********************************************************************
function ASAE_FSDK_GetVersion(
  //[in] pointer to the engine:
  hEngine: MHandle
  ): ASAE_FSDK_Version; Cdecl; external ArcAgeEstimationDll;

implementation

end.
