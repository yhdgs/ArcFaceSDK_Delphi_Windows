unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, System.Generics.Collections, hyiedefs,
  hyieutils,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, ArcFaceSDK, ArcFaceSDKIEVersion,
  Vcl.StdCtrls, ieview, imageenview, arcsoft_fsdk_face_recognition,
  Vcl.ExtCtrls;

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
  private
    FArcIESDK: TArcFaceSDKIEVersion;
    FFaceModels1, FFaceModels2: TFaceModels;
    FFaceInfo1, FFaceInfo2: TList<TFaceBaseInfo>;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}


procedure TForm1.btn1Click(Sender: TObject);
var
  i: Integer;
begin
  if OpenDialog1.Execute then
  begin
    FFaceInfo1.Clear;
    FFaceModels1.Clear;
    //检测并提取人脸特征
    FArcIESDK.DRAGfromFile(OpenDialog1.FileName,
      FFaceInfo1, FFaceModels1, ImageEnView1.IEBitmap, 800, 0,
      rfLanczos3, 0, False);

    //画人脸框
    for i := 0 to FFaceInfo1.Count - 1 do
      FArcIESDK.DrawFaceRectAgeGender(ImageEnView1, i + 1, FFaceInfo1.Items[i],
        clBlue, 3, true, 12);

    if (FFaceModels1.Count > 0) and (FFaceModels2.Count > 0) then
      lbl1.Caption := Format('相似度：%.2f%%',
        [FArcIESDK.MatchFace(FFaceModels1.Items[0],
        FFaceModels2.Items[0]) * 100]);
  end;
end;

procedure TForm1.btn2Click(Sender: TObject);
var
  i: Integer;
begin
  if OpenDialog1.Execute then
  begin
    FFaceInfo2.Clear;
    FFaceModels2.Clear;
    FArcIESDK.DRAGfromFile(OpenDialog1.FileName,
      FFaceInfo2, FFaceModels2, ImageEnView2.IEBitmap, 800, 0,
      rfLanczos3, 0, False);
    for i := 0 to FFaceInfo2.Count - 1 do
      FArcIESDK.DrawFaceRectAgeGender(ImageEnView2, i + 1, FFaceInfo2.Items[i]);
    if (FFaceModels1.Count > 0) and (FFaceModels2.Count > 0) then
      lbl1.Caption := Format('相似度：%.2f%%',
        [FArcIESDK.MatchFace(FFaceModels1.Items[0],
        FFaceModels2.Items[0]) * 100]);
  end;

end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FFaceModels1.Free;
  FFaceModels2.Free;
  FFaceInfo1.Free;
  FFaceInfo2.Free;
  FArcIESDK.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  //创建人脸识别SDK封装组件
  FArcIESDK := TArcFaceSDKIEVersion.Create;
  //创建人脸位置框信息列表
  FFaceModels1 := TFaceModels.Create;
  FFaceModels2 := TFaceModels.Create;
  //创建人脸位置框信息列表
  FFaceInfo1 := TList<TFaceBaseInfo>.Create;
  FFaceInfo2 := TList<TFaceBaseInfo>.Create;

  with FArcIESDK do
  begin
    //MaxFace := 10;
    //Scale := 16;
    //初始化人脸追踪引擎
    InitialFaceTrackingEngine(False);
    //初始化人脸检测引擎
    InitialFaceDetectionEngine(False);
    //初始化人脸特征提取引擎
    InitialFaceRecognitionEngine(False);
    //初始化年龄识别引擎
    InitialFaceAgeEngine(False);
    //初始化性别识别引擎
    InitialFaceGenderEngine(False);
  end;
end;

end.
