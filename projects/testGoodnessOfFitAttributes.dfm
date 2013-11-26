object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Goodness of Fit for attributes'
  ClientHeight = 621
  ClientWidth = 1167
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 341
    Width = 1167
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    ExplicitTop = 0
    ExplicitWidth = 462
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 1167
    Height = 41
    Align = alTop
    TabOrder = 0
    object edtPath: TEdit
      Left = 8
      Top = 8
      Width = 417
      Height = 21
      Color = clSilver
      TabOrder = 0
    end
    object btnOpenFile: TButton
      Left = 429
      Top = 7
      Width = 75
      Height = 23
      Caption = 'Open File'
      TabOrder = 1
      OnClick = btnOpenFileClick
    end
    object btnCalcGoodness: TButton
      Left = 510
      Top = 6
      Width = 101
      Height = 23
      Caption = 'Goodness of Fit'
      Enabled = False
      TabOrder = 2
      OnClick = btnCalcGoodnessClick
    end
    object chkverbosity: TCheckBox
      Left = 632
      Top = 9
      Width = 209
      Height = 17
      Caption = 'output with verbosity'
      TabOrder = 3
    end
  end
  object sgAttributes: TStringGrid
    Left = 0
    Top = 41
    Width = 1167
    Height = 300
    Align = alClient
    DefaultColWidth = 80
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing]
    TabOrder = 1
    OnDrawCell = sgAttributesDrawCell
    OnSelectCell = sgAttributesSelectCell
  end
  object log: TMemo
    Left = 0
    Top = 344
    Width = 1167
    Height = 277
    Align = alBottom
    Color = clBlack
    Font.Charset = ANSI_CHARSET
    Font.Color = clLime
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 2
    WordWrap = False
  end
  object ComboBox1: TComboBox
    Left = 957
    Top = 8
    Width = 145
    Height = 21
    Style = csDropDownList
    TabOrder = 3
    OnChange = ComboBox1Change
    OnEnter = ComboBox1Enter
    OnExit = ComboBox1Exit
  end
  object OpenDialog1: TOpenDialog
    Left = 1048
    Top = 40
  end
end
