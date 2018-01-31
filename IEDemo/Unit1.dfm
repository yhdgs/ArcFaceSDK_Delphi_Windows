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
    BevelOuter = bvNone
    Caption = 'pnl1'
    TabOrder = 0
    object ImageEnView1: TImageEnView
      Left = 0
      Top = 0
      Width = 361
      Height = 502
      Background = clBtnFace
      ParentCtl3D = False
      AutoFit = True
      EnableInteractionHints = True
      Align = alClient
      TabOrder = 0
    end
    object pnl3: TPanel
      Left = 0
      Top = 502
      Width = 361
      Height = 41
      Align = alBottom
      BevelOuter = bvNone
      Caption = 'pnl3'
      ShowCaption = False
      TabOrder = 1
      DesignSize = (
        361
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
    BevelOuter = bvNone
    Caption = 'pnl1'
    TabOrder = 1
    object pnl4: TPanel
      Left = 0
      Top = 502
      Width = 378
      Height = 41
      Align = alBottom
      BevelOuter = bvNone
      Caption = 'pnl4'
      ShowCaption = False
      TabOrder = 0
      DesignSize = (
        378
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
    object ImageEnView2: TImageEnView
      Left = 0
      Top = 0
      Width = 378
      Height = 502
      Background = clBtnFace
      ParentCtl3D = False
      AutoFit = True
      EnableInteractionHints = True
      Align = alClient
      TabOrder = 1
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
    Left = 712
    Top = 120
  end
end
