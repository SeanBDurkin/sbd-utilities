object mfmStringReplacer: TmfmStringReplacer
  Left = 590
  Top = 337
  BorderStyle = bsSingle
  Caption = 'String Replacer'
  ClientHeight = 75
  ClientWidth = 384
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
  DesignSize = (
    384
    75)
  PixelsPerInch = 96
  TextHeight = 13
  object lblTarget: TTntLabel
    Left = 8
    Top = 0
    Width = 50
    Height = 13
    Caption = 'Target file:'
    FocusControl = edtTarget
  end
  object lblSubstitution: TTntLabel
    Left = 8
    Top = 24
    Width = 46
    Height = 13
    Caption = 'Subst. ini:'
    FocusControl = edtSubstitution
  end
  object edtTarget: TTntEdit
    Left = 64
    Top = 0
    Width = 313
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    Text = '[Target file name]'
  end
  object edtSubstitution: TTntEdit
    Left = 64
    Top = 24
    Width = 313
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    Text = '[Substition ini file]'
  end
  object btnProceed: TButton
    Left = 8
    Top = 48
    Width = 145
    Height = 25
    Caption = 'Proceed with substitution'
    TabOrder = 2
    OnClick = btnProceedClick
  end
  object appevMain: TApplicationEvents
    OnIdle = appevMainIdle
    Left = 168
    Top = 32
  end
end
