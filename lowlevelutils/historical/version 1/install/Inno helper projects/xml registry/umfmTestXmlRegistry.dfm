object mfmTestXmlRegistry: TmfmTestXmlRegistry
  Left = 0
  Top = 0
  Width = 347
  Height = 125
  Caption = 'Test program for XmlRegistry'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  DesignSize = (
    339
    91)
  PixelsPerInch = 96
  TextHeight = 13
  object lblXMLFile: TLabel
    Left = 0
    Top = 0
    Width = 40
    Height = 13
    Caption = 'XML file:'
    FocusControl = edtXMLFile
  end
  object lblKey: TLabel
    Left = 0
    Top = 32
    Width = 22
    Height = 13
    Caption = 'Key:'
    FocusControl = edtKey
  end
  object lblResult: TLabel
    Left = 88
    Top = 64
    Width = 111
    Height = 13
    Caption = '[Result displayed here]'
  end
  object edtXMLFile: TEdit
    Left = 40
    Top = 0
    Width = 217
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    Text = 
      'C:\Documents and Settings\Developer\Local Settings\Application D' +
      'ata\Borland\BDS\3.0\DefProject.bdsproj'
  end
  object btnBrowseXMLFile: TBitBtn
    Left = 264
    Top = 0
    Width = 75
    Height = 25
    Action = actBrowseXMLFile
    Anchors = [akTop, akRight]
    Caption = 'Browse'
    TabOrder = 1
  end
  object edtKey: TEdit
    Left = 40
    Top = 32
    Width = 297
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
    Text = 
      'BorlandProject\Delphi.Personality\Directories\Directories\\Name=' +
      '"UnitOutputDir"'
  end
  object btnParse: TButton
    Left = 8
    Top = 56
    Width = 75
    Height = 25
    Action = actParse
    TabOrder = 3
  end
  object dlgOpenXMLFile: TOpenDialog
    DefaultExt = '.xml'
    Filter = 
      'Generic xml file|*.xml|Borland Project File|*.bdsproj|Any file|*' +
      '.*'
    Title = 'Select xml file to parse'
    Left = 304
    Top = 56
  end
  object actmngrTestXML: TActionManager
    Left = 272
    Top = 56
    StyleName = 'XP Style'
    object actBrowseXMLFile: TAction
      Caption = 'Browse'
      OnExecute = actBrowseXMLFileExecute
    end
    object actParse: TAction
      Caption = 'Parse'
      OnExecute = actParseExecute
    end
  end
end
