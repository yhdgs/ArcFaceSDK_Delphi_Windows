object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 584
  ClientWidth = 742
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object spl1: TSplitter
    Left = 361
    Top = 41
    Height = 543
    ExplicitLeft = 384
    ExplicitTop = 184
    ExplicitHeight = 100
  end
  object pnl1: TPanel
    Left = 0
    Top = 41
    Width = 361
    Height = 543
    Align = alLeft
    ShowCaption = False
    TabOrder = 0
    object img1: TImage
      Left = 1
      Top = 1
      Width = 359
      Height = 500
      Align = alClient
      Stretch = True
      ExplicitLeft = 80
      ExplicitTop = 88
      ExplicitWidth = 105
      ExplicitHeight = 105
    end
    object pnl3: TPanel
      Left = 1
      Top = 501
      Width = 359
      Height = 41
      Align = alBottom
      BevelOuter = bvNone
      Caption = 'pnl3'
      ShowCaption = False
      TabOrder = 0
      ExplicitLeft = 0
      ExplicitTop = 502
      ExplicitWidth = 361
      DesignSize = (
        359
        41)
      object btn1: TButton
        Left = 24
        Top = 8
        Width = 75
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = #36733#20837
        TabOrder = 0
        OnClick = btn1Click
      end
    end
  end
  object pnl2: TPanel
    Left = 364
    Top = 41
    Width = 378
    Height = 543
    Align = alClient
    Caption = 'pnl1'
    ShowCaption = False
    TabOrder = 1
    object img2: TImage
      Left = 1
      Top = 1
      Width = 376
      Height = 500
      Align = alClient
      Center = True
      Stretch = True
      ExplicitLeft = 88
      ExplicitTop = 96
      ExplicitWidth = 105
      ExplicitHeight = 105
    end
    object pnl4: TPanel
      Left = 1
      Top = 501
      Width = 376
      Height = 41
      Align = alBottom
      BevelOuter = bvNone
      Caption = 'pnl4'
      ShowCaption = False
      TabOrder = 0
      ExplicitLeft = 0
      ExplicitTop = 502
      ExplicitWidth = 378
      DesignSize = (
        376
        41)
      object btn2: TButton
        Left = 28
        Top = 8
        Width = 75
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = #36733#20837
        TabOrder = 0
        OnClick = btn2Click
      end
    end
  end
  object pnl5: TPanel
    Left = 0
    Top = 0
    Width = 742
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    object lbl1: TLabel
      Left = 24
      Top = 10
      Width = 58
      Height = 25
      Caption = '0.0%'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -21
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
  end
  object OpenDialog1: TOpenDialog
    Left = 344
    Top = 120
  end
end
