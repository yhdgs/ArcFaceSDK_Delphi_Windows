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
unit arcsoft_fsdk_gender_estimation;

interface

uses
  Windows, Messages, SysUtils, Classes, amcomdef, asvloffscreendef;

const

  ArcGenderEstimationDll = 'libarcsoft_fsdk_gender_estimation.dll';

type
  //This enumeration defines the orientation of the face in anti-clockwise sequence.
  ASGE_FSDK_GenderOrientCode =
    (
    ASGE_FSDK_FOC_Gender_0 = $1, //0 degree
    ASGE_FSDK_FOC_Gender_90 = $2, //90 degree
    ASGE_FSDK_FOC_Gender_270 = $3, //270 degree
    ASGE_FSDK_FOC_Gender_180 = $4, //180 degree
    ASGE_FSDK_FOC_Gender_30 = $5, //30 degree
    ASGE_FSDK_FOC_Gender_60 = $6, //60 degree
    ASGE_FSDK_FOC_Gender_120 = $7, //120 degree
    ASGE_FSDK_FOC_Gender_150 = $8, //150 degree
    ASGE_FSDK_FOC_Gender_210 = $9, //210 degree
    ASGE_FSDK_FOC_Gender_240 = $A, //240 degree
    ASGE_FSDK_FOC_Gender_300 = $B, //300 degree
    ASGE_FSDK_FOC_Gender_330 = $C//330 degree,
    );
  //ASGE_FSDK_FOC_Gender_0 .. ASGE_FSDK_FOC_Gender_330;
  //{$EXTERNALSYM ASGE_FSDK_GenderOrientCode}

  ASGE_FSDK_GENDERFACEINPUT = record
    //Bounding boxes of input faces.
    //MRECT * pFaceRectArray;
    pFaceRectArray: PMRECT;
    //Orientations of input faces. Can be set as one item of "ASGE_FSDK_GenderOrientCode".
    //MInt32 * pFaceOrientArray;
    pFaceOrientArray: PINT;
    //Number of input faces, and it must be greater than or euqual to 0.
    lFaceNumber: MInt32;
  end;

  LPASGE_FSDK_GENDERFACEINPUT = ^ASGE_FSDK_GENDERFACEINPUT;

  ASGE_FSDK_GENDERRESULT = record
    //The gender aResult aArray with length of "lFaceNumber".
    //The value will be equal to one of below: 0, 1 and -1.
    //"1" represents female, "0" represents male, and "-1" represents unknown.*/
    //MInt32 * pGenderResultArray;
    pGenderResultArray: IntPtr;
    //It is the same as "lFaceNumber" in "ASGE_FSDK_GENDERFACEINPUT".
    lFaceNumber: MInt32;

  end;

  LPASGE_FSDK_GENDERRESULT = ^ASGE_FSDK_GENDERRESULT;

  ASGE_FSDK_Version = record
    lCodebase: MInt32; //code base version number
    lMajor: MInt32; //major version number
    lMinor: MInt32; //minor version number
    lBuild: MInt32; //build version number, increasable only
    Version: MPChar; //version in string form
    BuildDate: MPChar; //latest build date
    CopyRight: MPChar; //copyright
  end;

  TASGE_FSDK_Version = ASGE_FSDK_Version;
{$EXTERNALSYM TASGE_FSDK_Version}
  // **********************************************************************
  //The function is used to initialize the gender estimation module.
  // **********************************************************************
function ASGE_FSDK_InitGenderEngine(
  AppId: MPChar; //[in]  APPID
  SDKKey: MPChar; //[in]  SDKKEY
  pMem: PByte; //[in]	 User allocated memory for the engine
  lMemSize: MInt32; //[in]	 User allocated memory size
  var hEngine: MHandle//[out] Pointer pointing to the gender estimation engine
  ): MRESULT; Cdecl; external ArcGenderEstimationDll;

// *****************************************************************************
//The function is used to estimate gender on static image mode automatically.
// *****************************************************************************
function ASGE_FSDK_GenderEstimation_StaticImage(
  hEngine: MHandle; //[in]  The gender estimation engine
  pImgInfo: LPASVLOFFSCREEN; //[in]  The original image data
  pFaceRes: LPASGE_FSDK_GENDERFACEINPUT; //[in]  The face position
  var AGenderRes: ASGE_FSDK_GENDERRESULT//,	// [out] The detection aResult
  ): MRESULT; Cdecl; external ArcGenderEstimationDll;

// ************************************************************************
//The function is used to estimate gender on preview mode automatically.
// ************************************************************************
function ASGE_FSDK_GenderEstimation_Preview(
  hEngine: MHandle; //[in]  The gender estimation engine
  pImgInfo: LPASVLOFFSCREEN; //[in]  The original image data
  FaceRes: LPASGE_FSDK_GENDERFACEINPUT; //[in]  The face position
  var AGenderRes: ASGE_FSDK_GENDERRESULT//,	// [out] The detection aResult
  ): MRESULT; Cdecl; external ArcGenderEstimationDll;

// ***********************************************************************
//The function is used to release the gender estimation module.
// ***********************************************************************
function ASGE_FSDK_UninitGenderEngine(
  hEngine: MHandle//[in]  Pointer pointing to the gender estimation engine
  ): MRESULT; Cdecl; external ArcGenderEstimationDll;

// ***************************************************************************
//The function used to get version information of gender estimation library.
// ***************************************************************************
function ASGE_FSDK_GetVersion(
  //[in]  Pointer pointing to the gender estimation engine:
  hEngine: MHandle): ASGE_FSDK_Version; Cdecl; external ArcGenderEstimationDll;

implementation

end.
