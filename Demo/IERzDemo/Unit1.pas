unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, forms,
  System.Classes, Vcl.Graphics, System.Generics.Collections, hyiedefs,
  hyieutils, iexBitmaps, iesettings, iexLayers, iexRulers, ArcFaceSDKIEVersion,
  arcsoft_fsdk_fic, Vcl.Dialogs, Vcl.StdCtrls, ieview, imageenview,
  Vcl.Controls, Vcl.ExtCtrls, arcsoft_fsdk_face_recognition;

type
  TForm1 = class(TForm)
    OpenDialog1: TOpenDialog;
    pnl1: TPanel;
    pnl2: TPanel;
    ImageEnView1: TImageEnView;
    pnl3: TPanel;
    btn1: TButton;
    pnl4: TPanel;
    btn2: TButton;
    ImageEnView2: TImageEnView;
    spl1: TSplitter;
    pnl5: TPanel;
    lbl1: TLabel;
    procedure btn1Click(Sender: TObject);
    procedure btn2Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    FArcIESDK: TArcFaceSDKIEVersion;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}


procedure TForm1.btn1Click(Sender: TObject);
var
  lSimilarScore: Single;
  lCompareResult: Integer;
begin
  if OpenDialog1.Execute then
  begin
    FArcIESDK.RzFicIdCardDataFeatureExtractionFromFile(OpenDialog1.FileName,
      nil, 0, 0, rfNone, 0);
    ImageEnView1.IO.LoadFromFile(OpenDialog1.FileName);
    ImageEnView1.Update;
    FArcIESDK.RzFicFaceIdCardCompare(lSimilarScore, lCompareResult, 0.82);
    lbl1.Caption := Format('%d - %f', [lCompareResult, lSimilarScore]);
  end;
end;

procedure TForm1.btn2Click(Sender: TObject);
var
  lSimilarScore: Single;
  lCompareResult: Integer;
  lficface: AFIC_FSDK_FACERES;
  lfaceinput: AFR_FSDK_FACEINPUT;
begin
  if OpenDialog1.Execute then
  begin
    FArcIESDK.RzFicFaceDataFeatureExtractionFromFile(OpenDialog1.FileName,
      False, lficface, nil, 0, 0, rfNone, 0, False);
    ImageEnView2.IO.LoadFromFile(OpenDialog1.FileName);
    ImageEnView2.Update;
    FArcIESDK.RzFicFaceIdCardCompare(lSimilarScore, lCompareResult, 0.82);
    lbl1.Caption := Format('%d - %f', [lCompareResult, lSimilarScore]);
    FArcIESDK.DrawFaceRectEX(ImageEnView2, 0, lficface.rcFace, clRed,
      2, False, 1, 12, True, 230, clBlue);
  end;

end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FArcIESDK.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  // 创建人脸识别SDK封装组件
  FArcIESDK := TArcFaceSDKIEVersion.Create;
  with FArcIESDK do
  begin
    // 初始化人证SDK引擎
    InitialFaceRzFicEngine(False);
  end;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  pnl1.Width := Round(Self.Width / 2);
end;

end.
