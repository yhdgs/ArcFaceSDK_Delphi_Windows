(* *******************************************************************************
  * C to Pascal by NJTZ 2017.10.10 eMail:yhdgs@qq.com
  ******************************************************************************* *)

unit ammemDef;

interface

uses
  Windows, Messages, SysUtils, Classes, amcomdef;

type
  __tag_mem_info = record
    dwTotalMemUsed: MDWord;
    dwTotalMemFree: MDWord;
  end;

  MMEMINFO = __tag_mem_info;
{$EXTERNALSYM MMEMINFO}
  LPMMEMINFO = ^__tag_mem_info;
{$EXTERNALSYM LPMMEMINFO}
  {
    function MMemAlloc(hContext: MHandle; lSize: MLong): MVoid;
    function MMemRealloc(hContext: MHandle; var pMem: MVoid; lSize: MLong): MVoid;
    function MMemFree(hContext: MHandle; var pMem: MVoid): MVoid;
    function MMemSet(var pMem: MVoid; byVal: MByte; lSize: MLong): MVoid;
    function MMemCpy(var pDst: MVoid; var pSrc: MVoid; lSize: MLong): MVoid;
    function MMemMove(var pDst: MVoid; var pSrc: MVoid; lSize: MLong): MVoid;
    function MMemCmp(var pBuf1: MVoid; var pBuf2: MVoid; lSize: MLong): MLong;
    function MMemMgrCreate(var pMem: MVoid; lMemSize: MLong): MHandle;
    function MMemMgrDestroy(hMemMgr: MHandle): MVoid;
    function GetMaxAllocMemSize(hContext: MHandle): MLong;
  }

  MPBASE_Version = record
    lCodebase: MLong; //Codebase version number
    lMajor: MLong; //major version number
    lMinor: MLong; //minor version number
    lBuild: MLong; //Build version number, increasable only
    Version: PAnsiChar; //version in string form
    BuildDate: PAnsiChar; //latest build Date
    CopyRight: PAnsiChar; //copyright
  end;

  TMPBASE_Version = MPBASE_Version;
{$EXTERNALSYM TMPBASE_Version}
  //function Mpbase_GetVersion(): MPBASE_Version;

implementation

end.
