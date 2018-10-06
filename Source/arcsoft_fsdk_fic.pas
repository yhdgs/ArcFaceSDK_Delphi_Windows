(* *******************************************************************************
  * C to Pascal by NJTZ 2018.9.20 eMail:yhdgs@qq.com
  ******************************************************************************** *)

unit arcsoft_fsdk_fic;

interface

uses
  Windows, Messages, SysUtils, Classes, amcomdef, asvloffscreenDef;

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
  ******************************************************************************** *)

const
  ArcFicDll = 'libarcsoft_fsdk_fic.dll';

type
  (* ******************************************************************************************
    * FIC 版本信息
    ****************************************************************************************** *)
  AFIC_FSDK_VERSION = record
    lCodebase: MInt32; // Codebase version number
    lMajor: MInt32; // Major version number
    lMinor: MInt32; // Minor version number
    lBuild: MInt32; // Build version number, increasable only
    Version: MPChar; // Version in string form
    BuildDate: MPChar; // Latest build Date
    CopyRight: MPChar; // Copyright
  end;

  LPAFIC_FSDK_VERSION = ^AFIC_FSDK_VERSION;

  (* ******************************************************************************************
    * FIC FT/FD人脸特征检测
    ****************************************************************************************** *)

  AFIC_FSDK_FACERES = record
    nFace: MInt32; // number of faces detected
    rcFace: MRECT; // IntPtr; // PMRECT; //The bounding box of face
  end;

  LPAFIC_FSDK_FACERES = ^AFIC_FSDK_FACERES;

  (* ***********************************************************************
    * 初始化引擎
    *********************************************************************** *)
function ArcSoft_FIC_InitialEngine(ID: MPChar; // [in] APPID
  SDKKEY: MPChar; // [in] SDKKEY
  var phFICEngine: MHandle // [out] FIC 引擎Handle的指针
  ): MRESULT; Cdecl; external ArcFicDll;

(* ***********************************************************************
  * 人脸特征提取
  *********************************************************************** *)
function ArcSoft_FIC_FaceDataFeatureExtraction(hFICEngine: MHandle;
  // [in]  FIC 引擎Handle
  isVideo: MBool; // [in]  人脸数据类型 1-视频 0-静态图片
  pInputFaceData: LPASVLOFFSCREEN; // [in]  人脸图像原始数据
  // pFaceRes: LPAFIC_FSDK_FACERES
  var FaceRes: AFIC_FSDK_FACERES // [out] 人脸属性 人脸数/人脸框/角度
  ): MRESULT; Cdecl; external ArcFicDll;

(* ***********************************************************************
  * 证件照特征提取
  *********************************************************************** *)
function ArcSoft_FIC_IdCardDataFeatureExtraction(hFICEngine: MHandle;
  // [in]  FIC 引擎Handle
  pInputIdcardData: LPASVLOFFSCREEN // [in]  图像原始数据
  ): MRESULT; Cdecl; external ArcFicDll;

(* ***********************************************************************
  * 人证比对
  *********************************************************************** *)
function ArcSoft_FIC_FaceIdCardCompare(hFICEngine: MHandle; // [in] FIC 引擎Handle
  threshold: MFloat; // [in]  比对阈值
  var pSimilarScore: MFloat; // [out] 比对结果相似度
  var pResult: MInt32 // [out] 比对结果
  ): MRESULT; Cdecl; external ArcFicDll;

(* ***********************************************************************
  * 释放引擎
  *********************************************************************** *)
function ArcSoft_FIC_UninitialEngine(hFICEngine: MHandle // [in] FIC 引擎Handle
  ): MRESULT; Cdecl; external ArcFicDll;

(* ***********************************************************************
  * 获取版本信息
  *********************************************************************** *)
function ArcSoft_FIC_GetVersion(hFICEngine: MHandle): LPAFIC_FSDK_VERSION;
  Cdecl; external ArcFicDll;

implementation

end.
