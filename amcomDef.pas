(* *******************************************************************************
  *
  * This aFile is ArcSoft's property. It contains ArcSoft's trade secret, proprietary and
  * confidential information.
  *
  * The information and code contained in this aFile is only for authorized ArcSoft employees
  * to design, create, modify, or review.
  *
  * DO NOT DISTRIBUTE, DO NOT DUPLICATE OR TRANSMIT  ANY FORM WITHOUT PROPER AUTHORIZATION.
  *
  * If you are not an intended recipient of this aFile, you must not copy, distribute, modify,
  * or take any action in reliance on it.
  *
  * If you have received this aFile in error, please immediately notify ArcSoft and
  * permanently delete the original and any copy of any aFile and any printout thereof.
  * *******************************************************************************
  * C to Pascal by NJTZ 2017.10.10 eMail:yhdgs@qq.com
  ******************************************************************************** *)

unit amcomdef;

interface

uses
  Windows, Messages, SysUtils, Classes;

type
{$IFDEF UNICODE}
  MWChar = WideChar;
{$EXTERNALSYM MWChar}
{$ELSE}
  MWChar = Word;
{$EXTERNALSYM MWChar}
{$ENDIF}

{$IFDEF UNICODE}
  MTChar = MWChar;
{$EXTERNALSYM MTChar}
{$ELSE}
  MTChar = MChar;
{$EXTERNALSYM MTChar}
{$ENDIF}
  MLong = LongInt;
{$EXTERNALSYM MLong}
  MFloat = Single;
{$EXTERNALSYM MFloat}
  MDouble = Double;
{$EXTERNALSYM MDouble}
  MByte = Byte;
{$EXTERNALSYM MByte}
  MWord = Word;
{$EXTERNALSYM MWord}
  MDWord = Cardinal;
{$EXTERNALSYM MDWord}
  MHandle = Pointer;
{$EXTERNALSYM MHandle}
  MChar = AnsiChar;
{$EXTERNALSYM MChar}
  MBool = LongInt;
{$EXTERNALSYM MBool}
  MVoid = Pointer;
{$EXTERNALSYM MVoid}
  MPVoid = Pointer;
{$EXTERNALSYM MPVoid}
  MPChar = PAnsiChar;
{$EXTERNALSYM MPChar}
  MShort = SmallInt;
{$EXTERNALSYM MShort}
  MPCChar = PChar;
{$EXTERNALSYM MPCChar}
  MRESULT = MLong;
{$EXTERNALSYM MRESULT}
  MCOLORREF = MDWord;
{$EXTERNALSYM MCOLORREF}
  MInt8 = ShortInt;
{$EXTERNALSYM MInt8}
  MUInt8 = Byte;
{$EXTERNALSYM MUInt8}
  MInt16 = SmallInt;
{$EXTERNALSYM MInt16}
  MUInt16 = Word;
{$EXTERNALSYM MUInt16}
  MInt32 = Integer;
{$EXTERNALSYM MInt32}
  MUInt32 = Cardinal;
{$EXTERNALSYM MUInt32}
  MInt64 = Int64;
{$EXTERNALSYM MInt64}
  MUInt64 = Cardinal;
{$EXTERNALSYM MUInt64}

  __tag_rect = record
    left: MInt32;
    top: MInt32;
    right: MInt32;
    bottom: MInt32;
  end;

  MRECT = __tag_rect;
{$EXTERNALSYM MRECT}
  PMRECT = ^__tag_rect;
{$EXTERNALSYM PMRECT}

  __tag_point = record
    x: MInt32;
    y: MInt32;
  end;

  MPOINT = __tag_point;
{$EXTERNALSYM MPOINT}
  PMPOINT = ^__tag_point;
{$EXTERNALSYM PMPOINT}


const
  MNull = 0;
{$EXTERNALSYM MNull}
  MFalse = 0;
{$EXTERNALSYM MFalse}
  MTrue = 1;
{$EXTERNALSYM MTrue}
  MAX_PATH = 256;
{$EXTERNALSYM MAX_PATH}


implementation

end.
