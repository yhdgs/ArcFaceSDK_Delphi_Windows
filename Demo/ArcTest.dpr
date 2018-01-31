program ArcTest;

uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  amcomDef in '..\amcomDef.pas',
  ammemDef in '..\ammemDef.pas',
  ArcFaceSDK in '..\ArcFaceSDK.pas',
  ArcFaceSDKIEVersion in '..\ArcFaceSDKIEVersion.pas',
  arcsoft_fsdk_face_detection in '..\arcsoft_fsdk_face_detection.pas',
  arcsoft_fsdk_face_recognition in '..\arcsoft_fsdk_face_recognition.pas',
  arcsoft_fsdk_face_tracking in '..\arcsoft_fsdk_face_tracking.pas',
  asvloffscreenDef in '..\asvloffscreenDef.pas',
  merrorDef in '..\merrorDef.pas',
  arcsoft_fsdk_age_estimation in '..\arcsoft_fsdk_age_estimation.pas',
  arcsoft_fsdk_gender_estimation in '..\arcsoft_fsdk_gender_estimation.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
