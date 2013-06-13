object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Thundax Test Matrix'
  ClientHeight = 505
  ClientWidth = 876
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 8
    Top = 39
    Width = 860
    Height = 458
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    WordWrap = False
  end
  object Button1: TButton
    Left = 793
    Top = 8
    Width = 75
    Height = 25
    Caption = 'test'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 682
    Top = 8
    Width = 105
    Height = 25
    Caption = 'test covariance'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 592
    Top = 8
    Width = 84
    Height = 25
    Caption = 'Test Examples'
    TabOrder = 3
    OnClick = Button3Click
  end
end
