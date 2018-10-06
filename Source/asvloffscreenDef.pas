unit asvloffscreendef;

interface

uses
    Windows, Messages, SysUtils, Classes, amcomdef;

const

  {31 30 29 28 27 26 25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10 09 08 07 06 05 04 03 02 01 00}

  {R  R  R  R  R  G  G  G  G  G  G  B  B  B  B  B}
  ASVL_PAF_RGB16_B5G6R5 = $101;
  {X  R  R  R  R  R  G  G  G  G  G  B  B  B  B  B}
  ASVL_PAF_RGB16_B5G5R5 = $102;
  {X  X  X  X  R  R  R  R  G  G  G  G  B  B  B  B}
  ASVL_PAF_RGB16_B4G4R4 = $103;
  {T  R  R  R  R  R  G  G  G  G  G  B  B  B  B  B}
  ASVL_PAF_RGB16_B5G5R5T = $104;
  {B  B  B  B  B  G  G  G  G  G  G  R  R  R  R  R}
  ASVL_PAF_RGB16_R5G6B5 = $105;
  {X  B  B  B  B  B  G  G  G  G  G  R  R  R  R  R}
  ASVL_PAF_RGB16_R5G5B5 = $106;
  {X  X  X  X  B  B  B  B  G  G  G  G  R  R  R  R}
  ASVL_PAF_RGB16_R4G4B4 = $107;

  {R	R  R  R	 R	R  R  R  G  G  G  G  G  G  G  G  B  B  B  B  B  B  B  B}
  ASVL_PAF_RGB24_B8G8R8 = $201;
  {X	X  X  X	 X	X  R  R  R  R  R  R  G  G  G  G  G  G  B  B  B  B  B  B}
  ASVL_PAF_RGB24_B6G6R6 = $202;
  {X	X  X  X	 X	T  R  R  R  R  R  R  G  G  G  G  G  G  B  B  B  B  B  B}
  ASVL_PAF_RGB24_B6G6R6T = $203;
  {B  B  B  B  B  B  B  B  G  G  G  G  G  G  G  G  R	R  R  R	 R	R  R  R}
  ASVL_PAF_RGB24_R8G8B8 = $204;
  {X	X  X  X	 X	X  B  B  B  B  B  B  G  G  G  G  G  G  R  R  R  R  R  R}
  ASVL_PAF_RGB24_R6G6B6 = $205;

  {X	X  X  X	 X	X  X  X	 R	R  R  R	 R	R  R  R  G  G  G  G  G  G  G  G  B  B  B  B  B  B  B  B}
  ASVL_PAF_RGB32_B8G8R8 = $301;
  {A	A  A  A	 A	A  A  A	 R	R  R  R	 R	R  R  R  G  G  G  G  G  G  G  G  B  B  B  B  B  B  B  B}
  ASVL_PAF_RGB32_B8G8R8A8 = $302;
  {X	X  X  X	 X	X  X  X	 B  B  B  B  B  B  B  B  G  G  G  G  G  G  G  G  R	R  R  R	 R	R  R  R}
  ASVL_PAF_RGB32_R8G8B8 = $303;
  {B    B  B  B  B  B  B  B  G  G  G  G  G  G  G  G  R  R  R  R  R  R  R  R  A	A  A  A  A	A  A  A}
  ASVL_PAF_RGB32_A8R8G8B8 = $304;
  {A    A  A  A  A  A  A  A  B  B  B  B  B  B  B  B  G  G  G  G  G  G  G  G  R  R  R  R  R	R  R  R}
  ASVL_PAF_RGB32_R8G8B8A8 = $305;

  {Y0, U0, V0}
  ASVL_PAF_YUV = $401;
  {Y0, V0, U0}
  ASVL_PAF_YVU = $402;
  {U0, V0, Y0}
  ASVL_PAF_UVY = $403;
  {V0, U0, Y0}
  ASVL_PAF_VUY = $404;

  {Y0, U0, Y1, V0}
  ASVL_PAF_YUYV = $501;
  {Y0, V0, Y1, U0}
  ASVL_PAF_YVYU = $502;
  {U0, Y0, V0, Y1}
  ASVL_PAF_UYVY = $503;
  {V0, Y0, U0, Y1}
  ASVL_PAF_VYUY = $504;
  {Y1, U0, Y0, V0}
  ASVL_PAF_YUYV2 = $505;
  {Y1, V0, Y0, U0}
  ASVL_PAF_YVYU2 = $506;
  {U0, Y1, V0, Y0}
  ASVL_PAF_UYVY2 = $507;
  {V0, Y1, U0, Y0}
  ASVL_PAF_VYUY2 = $508;
  {Y0, Y1, U0, V0}
  ASVL_PAF_YYUV = $509;

  {8 bit Y plane followed by 8 bit 2x2 subsampled U and V planes}
  ASVL_PAF_I420 = $601;
  {8 bit Y plane followed by 8 bit 1x2 subsampled U and V planes}
  ASVL_PAF_I422V = $602;
  {8 bit Y plane followed by 8 bit 2x1 subsampled U and V planes}
  ASVL_PAF_I422H = $603;
  {8 bit Y plane followed by 8 bit U and V planes}
  ASVL_PAF_I444 = $604;
  {8 bit Y plane followed by 8 bit 2x2 subsampled V and U planes}
  ASVL_PAF_YV12 = $605;
  {8 bit Y plane followed by 8 bit 1x2 subsampled V and U planes}
  ASVL_PAF_YV16V = $606;
  {8 bit Y plane followed by 8 bit 2x1 subsampled V and U planes}
  ASVL_PAF_YV16H = $607;
  {8 bit Y plane followed by 8 bit V and U planes}
  ASVL_PAF_YV24 = $608;
  {8 bit Y plane only}
  ASVL_PAF_GRAY = $701;

  {8 bit Y plane followed by 8 bit 2x2 subsampled UV planes}
  ASVL_PAF_NV12 = $801;
  {8 bit Y plane followed by 8 bit 2x2 subsampled VU planes}
  ASVL_PAF_NV21 = $802;
  {8 bit Y plane followed by 8 bit 2x1 subsampled UV planes}
  ASVL_PAF_LPI422H = $803;
  {8 bit Y plane followed by 8 bit 2x1 subsampled VU planes}
  ASVL_PAF_LPI422H2 = $804;

  {8 bit Y plane followed by 8 bit 4x4 subsampled VU planes}
  ASVL_PAF_NV41 = $805;

  {Negative UYVY, U0, Y0, V0, Y1}
  ASVL_PAF_NEG_UYVY = $901;
  {Negative I420, 8 bit Y plane followed by 8 bit 2x2 subsampled U and V planes}
  ASVL_PAF_NEG_I420 = $902;

  {Mono UYVY, UV values are fixed, gray image in U0, Y0, V0, Y1}
  ASVL_PAF_MONO_UYVY = $A01;
  {Mono I420, UV values are fixed, 8 bit Y plane followed by 8 bit 2x2 subsampled U and V planes}
  ASVL_PAF_MONO_I420 = $A02;

  {P8_YUYV, 8 pixels a group, Y0Y1Y2Y3Y4Y5Y6Y7U0U1U2U3V0V1V2V3}
  ASVL_PAF_P8_YUYV = $B03;

  {P16_YUYV, 16*16 pixels a group, Y0Y1Y2Y3...U0U1...V0V1...}
  ASVL_PAF_SP16UNIT = $C01;

  ASVL_PAF_DEPTH_U16 = $C02;

  {10 bits RGGB CFA raw data, each data has 2 bytes}

  ASVL_PAF_RAW10_RGGB_10B = $D01;
  ASVL_PAF_RAW10_GRBG_10B = $D02;
  ASVL_PAF_RAW10_GBRG_10B = $D03;
  ASVL_PAF_RAW10_BGGR_10B = $D04;

  ASVL_PAF_RAW12_RGGB_12B = $D05;
  ASVL_PAF_RAW12_GRBG_12B = $D06;
  ASVL_PAF_RAW12_GBRG_12B = $D07;
  ASVL_PAF_RAW12_BGGR_12B = $D08;

  ASVL_PAF_RAW10_RGGB_16B = $D09;
  ASVL_PAF_RAW10_GRBG_16B = $D0A;
  ASVL_PAF_RAW10_GBRG_16B = $D0B;
  ASVL_PAF_RAW10_BGGR_16B = $D0C;

  {10 bits gray raw data}
  ASVL_PAF_RAW10_GRAY_10B = $E01;

  {10 bits gray raw data, each data has 2 bytes}
  ASVL_PAF_RAW10_GRAY_16B = $E81;

type
  {Define the image format space}
  __tag_ASVL_OFFSCREEN = record
    u32PixelArrayFormat: MUInt32;
    i32Width: MInt32;
    i32Height: MInt32;
    ppu8Plane: array [0 .. 3] of IntPtr; // * ppu8Plane[4];
    pi32Pitch: array [0 .. 3] of MInt32;
  end;

  asvloffscreen = __tag_ASVL_OFFSCREEN;
{$EXTERNALSYM ASVLOFFSCREEN}
  LPASVLOFFSCREEN = ^__tag_ASVL_OFFSCREEN;
{$EXTERNALSYM LPASVLOFFSCREEN}

  {Define the SDK Version infos. This is the template!!!}
type
  __tag_ASVL_VERSION = record
    lCodebase: MLong; //Codebase version number
    lMajor: MLong; //major version number
    lMinor: MLong; //minor version number
    lBuild: MLong; //Build version number, increasable only
    Version: PAnsiChar; //version in string form
    BuildDate: PAnsiChar; //latest build Date
    CopyRight: PAnsiChar; //copyright
  end;

  ASVL_VERSION = __tag_ASVL_VERSION;
{$EXTERNALSYM ASVL_VERSION}

  //function ASVL_GetVersion(): ASVL_VERSION;

implementation

end.
