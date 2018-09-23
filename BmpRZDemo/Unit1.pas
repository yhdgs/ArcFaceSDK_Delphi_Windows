unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, System.Generics.Collections,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, ArcFaceSDK,
  Vcl.StdCtrls, arcsoft_fsdk_face_recognition,
  Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    OpenDialog1: TOpenDialog;
    pnl1: TPanel;
    pnl2: TPanel;
    pnl3: TPanel;
    btn1: TButton;
    pnl4: TPanel;
    btn2: TButton;
    spl1: TSplitter;
    pnl5: TPanel;
    lbl1: TLabel;
    img1: TImage;
    img2: TImage;
    btn3: TButton;
    procedure btn1Click(Sender: TObject);
    procedure btn2Click(Sender: TObject);
    procedure btn3Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    FArcSDK: TArcFaceSDK;
    FZjzBitmap, FRyzpBitmap: TBitmap;
  public
    procedure LoadImg(ABitMap: TBitmap; AImg: TImage);
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}


procedure TForm1.btn1Click(Sender: TObject);
begin
  LoadImg(FZjzBitmap, img1);
  FArcSDK.RzFicIdCardDataFeatureExtractionFromBmp(FZjzBitmap);
end;

procedure TForm1.btn2Click(Sender: TObject);
var
  lSimilarScore: Single;
  lCompareResult: Integer;
begin
  LoadImg(FRyzpBitmap, img2);
  FArcSDK.RzFicFaceDataFeatureExtractionFromBmp(FRyzpBitmap, False);
  FArcSDK.RzFicFaceIdCardCompare(lSimilarScore, lCompareResult, 0.82);
  lbl1.Caption := Format('%d - %f', [lCompareResult, lSimilarScore]);
end;

procedure TForm1.btn3Click(Sender: TObject);
var
  lSimilarScore: Single;
  lCompareResult: Integer;
begin
  FArcSDK.RzFicCompareFromBmp(FZjzBitmap, FRyzpBitmap, False, lSimilarScore,
    lCompareResult);
  lbl1.Caption := Format('%d - %f', [lCompareResult, lSimilarScore]);

end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FArcSDK.Free;
  FRyzpBitmap.Free;
  FZjzBitmap.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  // 创建人脸识别SDK封装组件
  FArcSDK := TArcFaceSDK.Create;
  FZjzBitmap := TBitmap.Create;
  FRyzpBitmap := TBitmap.Create;

  with FArcSDK do
  begin
    // MaxFace := 10;
    // Scale := 16;

    // 初始化人脸检测引擎
    InitialFaceRzFicEngine(False);
  end;

  OpenDialog1.Filter := '图片文件(*.jpg,*.jpeg,*.bmp)|*.jpg;*.jpeg;*.bmp';
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  pnl1.Width := Round(Self.Width / 2);
end;

procedure TForm1.LoadImg(ABitMap: TBitmap; AImg: TImage);
var
  i: Integer;
  sExt: string;
begin
  if OpenDialog1.Execute then
  begin
    sExt := ExtractFileExt(OpenDialog1.FileName);
    // 检测并提取人脸特征
    if (CompareText(sExt, '.jpg') = 0) or (CompareText(sExt, '.jpeg') = 0) then
      FArcSDK.ReadJpegFile(OpenDialog1.FileName, ABitMap)
    else if (CompareText(sExt, '.bmp') = 0) then
      FArcSDK.ReadBmpFile(OpenDialog1.FileName, ABitMap)
    else
      Exit;

    AImg.Picture.Assign(ABitMap);

  end;
end;

end.
