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
    procedure btn1Click(Sender: TObject);
    procedure btn2Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    FArcSDK: TArcFaceSDK;
    FFaceModels1, FFaceModels2: TFaceModels;
    FFaceInfo1, FFaceInfo2: TList<TFaceBaseInfo>;
  public
    procedure LoadImg(AImg: TImage; AFaceInfo: TList<TFaceBaseInfo>; AFaceModels:
        TFaceModels);
    {Public declarations}
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}


procedure TForm1.btn1Click(Sender: TObject);
begin
  LoadImg(img1, FFaceInfo1, FFaceModels1);
end;

procedure TForm1.btn2Click(Sender: TObject);
begin
  LoadImg(img2, FFaceInfo2, FFaceModels2);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FFaceModels1.Free;
  FFaceModels2.Free;
  FFaceInfo1.Free;
  FFaceInfo2.Free;
  FArcSDK.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  //创建人脸识别SDK封装组件
  FArcSDK := TArcFaceSDK.Create;
  //创建人脸位置框信息列表
  FFaceModels1 := TFaceModels.Create;
  FFaceModels2 := TFaceModels.Create;
  //创建人脸位置框信息列表
  FFaceInfo1 := TList<TFaceBaseInfo>.Create;
  FFaceInfo2 := TList<TFaceBaseInfo>.Create;

  with FArcSDK do
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

  OpenDialog1.Filter := '图片文件(*.jpg,*.jpeg,*.bmp)|*.jpg;*.jpeg;*.bmp';
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  pnl1.Width := Round(Self.Width / 2);
end;

procedure TForm1.LoadImg(AImg: TImage; AFaceInfo: TList<TFaceBaseInfo>;
    AFaceModels: TFaceModels);
var
  i: Integer;
  sExt: string;
  lBitmap: TBitmap;
begin
  if OpenDialog1.Execute then
  begin
    AFaceInfo.Clear;
    AFaceModels.Clear;

    lBitmap := TBitmap.Create;
    try
      sExt := ExtractFileExt(OpenDialog1.FileName);
      //检测并提取人脸特征
      if (CompareText(sExt, '.jpg') = 0) or (CompareText(sExt, '.jpeg') = 0)
      then
        FArcSDK.ReadJpegFile(OpenDialog1.FileName, lBitmap)
      else if (CompareText(sExt, '.bmp') = 0) then
        FArcSDK.ReadBmpFile(OpenDialog1.FileName, lBitmap)
      else
        Exit;

      FArcSDK.DRAGfromBmp(lBitmap, AFaceInfo, AFaceModels);

      AImg.Picture.Assign(lBitmap);

      //画人脸框
      for i := 0 to AFaceInfo.Count - 1 do
        FArcSDK.DrawFaceRectAgeGender(AImg.Canvas, i + 1,
          AFaceInfo.Items[i], clBlue, 3, true, 12);

      if (FFaceModels1.Count > 0) and (FFaceModels2.Count > 0) then
        lbl1.Caption := Format('相似度：%.2f%%',
          [FArcSDK.MatchFace(FFaceModels1.Items[0],
          FFaceModels2.Items[0]) * 100]);
    finally
      lBitmap.Free;
    end;
  end;
end;

end.
