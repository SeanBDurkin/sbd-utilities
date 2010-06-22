object mfmCommandConsolePervert: TmfmCommandConsolePervert
  Left = 166
  Top = 188
  Width = 601
  Height = 290
  Caption = 'Command Console Pervert'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PrintScale = poNone
  Scaled = False
  OnCreate = TntFormCreate
  OnDestroy = TntFormDestroy
  DesignSize = (
    593
    256)
  PixelsPerInch = 96
  TextHeight = 13
  object memoConsoleBox: TTntRichEdit
    Left = 168
    Top = 8
    Width = 425
    Height = 209
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      '[command lines will be in red]'
      '[response liines will be in green]')
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object lblIntro: TTntStaticText
    Left = 8
    Top = 8
    Width = 147
    Height = 241
    AutoSize = False
    BorderStyle = sbsSingle
    Caption = '[Introductory text goes here]'
    TabOrder = 1
  end
  object btnProceed: TBitBtn
    Left = 168
    Top = 224
    Width = 75
    Height = 25
    Action = actProceed
    Anchors = [akLeft, akBottom]
    Caption = 'Proceed'
    TabOrder = 2
    Glyph.Data = {
      36040000424D3604000000000000360000002800000010000000100000000100
      2000000000000004000000000000000000000000000000000000FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF0006640D0006640D0007690F0007690F000664
      0D0006640D00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF0006640D0006640D000A8C170009B0190009B31A0009B31A0009B0
      19000A97170007680E0007680E00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF0007690F00097D14000BB41D0009B31A0009B31A0009B0190009B31A0009B3
      1A0009B31A0009B31A00097D140006640D00FF00FF00FF00FF00FF00FF000769
      0F000A8C170016B32D0012B327000BB41D0009B0190009B0190009B0190009B0
      190009B0190009B31A0009B31A00097D140006650D00FF00FF00FF00FF000769
      0F0025B5490023B5440016B32D000BB41D001EB13600DDF6E700ECFAF1005BCE
      800009B0190009B0190009B31A0009B31A0006650D00FF00FF000872120020B0
      3B002EBC5C0023B544000EB421000AB41B000EB4210098E0AF00FBFDFC00F6FC
      F80055CC7B0009B0190009B0190009B31A000A97170006640D00087212002CBA
      590033C0640027B74E000EB4210009B31A000BB41D0009B019008DDDA500FBFD
      FC00F6FCF80055CC7B0009B0190009B31A0009AD190006640D00097D140039C3
      6B0039C36B00CEF1DB00C9EFD600C9EFD600C9EFD600C0EDCF00C0EDCF00F9FD
      FA00FBFDFC00F0FAF3006DD38F0009B31A0009B31A0006650D000A8C17005BCE
      800047C87500FBFDFC00FBFDFC00FBFDFC00FBFDFC00FBFDFC00FBFDFC00FBFD
      FC00FBFDFC00FBFDFC00B9EBC9000AB41B0009B31A0007690F000A8C17006DD3
      8F0067D18B006DD38F0075D6940075D6940075D6940067D18B008DDDA500FBFD
      FB00FBFDFC00A3E4B80027B74E0012B3270009B0190006640D000A8C170060CF
      8400A3E4B8003DC46E0036C268003DC46E003DC46E0075D69400E6F8ED00FBFD
      FC0098E0AF0025B5490022B13D0012B327000AA3180006640D00FF00FF0020B0
      3B00C0EDCF0085DAA00033C0640036C2680055CC7B00F7FBF900FBFDFC0098E0
      AF0025B5490023B5440019B2310016B32D000A8C1700FF00FF00FF00FF0020B0
      3B006DD38F00D3F3E00085DAA00039C36B003DC46E00C0EDCF00ADE7C00033C0
      640029B9550027B74E0023B5440019B231000A8C1700FF00FF00FF00FF00FF00
      FF0020B03B0085DAA000DDF6E700ADE7C0006DD38F0055CC7B0047C875004DCA
      78004DCA780039C36B0025B54900097D1400FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF0020B03B0020B03B00ADE7C000D0F2DD00C0EDCF00ADE7C00098E0
      AF006DD38F002CBA59002CBA5900FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF0027B74E0020B03B0020B03B0020B03B0020B0
      3B0023B34200FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
  end
  object btnFinish: TBitBtn
    Left = 248
    Top = 224
    Width = 75
    Height = 25
    Action = actFinish
    Anchors = [akLeft, akBottom]
    Caption = 'Finish'
    TabOrder = 3
    Glyph.Data = {
      36040000424D3604000000000000360000002800000010000000100000000100
      2000000000000004000000000000000000000000000000000000FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00B68A
      7F00A5787300A5787300A5787300A5787300A5787300A5787300A5787300A578
      7300A5787300A5787300A5787300986C6700FF00FF00FF00FF00FF00FF00B68A
      7F00FBF6F100E6E0EC00F0D1A600F0D1A600F0D1A600F0D1A600EFCFA400EDCC
      A100EDCCA100EDCCA100F0D1A600986C6700FF00FF00FF00FF00FF00FF00B483
      7800FBF6F1009833000098330000983300009833000098330000983300009833
      000098330000EDCCA100F0D1A600986C6700FF00FF00FF00FF00FF00FF00B483
      7800FCF7F10098330000FEFDFC00FEFDFC00FEFDFC008D90DF00E1C6EE00FEFD
      FC0098330000EECDA200F0D1A600986C6700FF00FF00FF00FF00FF00FF00BF94
      8400FEFCFA0098330000FEFDFC00FEFDFC008082EA000735FA00576CF900FEFD
      FC0098330000EFCFA400F0D1A600986C6700FF00FF00FF00FF00FF00FF00BF94
      8400FEFDFC0098330000E6E0EC004F64F9000735FA004F64F9000936FB00E6E0
      EC0098330000F0D1A600F0D1A600986C6700FF00FF00FF00FF00FF00FF00CC9A
      8100FEFDFC0098330000576CF9001137FB00E1C6EE00F2F1F7001938FA00576C
      F90097330200F0D1A600F0D1A600986C6700FF00FF00FF00FF00FF00FF00CC9A
      8100FEFDFC0098330000ECEAF400E6E0EC00FEFDFC00FEFDFC0099A2C3000735
      FA00675B5900F0D1A600F0D1A600986C6700FF00FF00FF00FF00FF00FF00DBA9
      8400FEFDFC0098330000FEFDFC00FEFDFC00FEFDFC00FEFDFC00FEFDFC00576C
      F9000735FA00F0D1A600F0D1A600986C6700FF00FF00FF00FF00FF00FF00DBA9
      8400FEFDFC009833000098330000983300009833000098330000983300008F33
      0E001C35F8000735FA00E3BF9A00986C6700FF00FF00FF00FF00FF00FF00DFB0
      8C00FEFDFC00FEFDFC00FEFDFC00FEFDFC00FEFDFC00FCF9F500FBF6F100E6E0
      EC00B48378000735FA000735FA000735FA00FF00FF00FF00FF00FF00FF00DFB0
      8C00FEFDFC00FEFDFC00FEFDFC00FEFDFC00FEFDFC00FEFDFC00FEFDFB00E4D7
      EA00B4837800DFB08C00DBA984000735FA00FF00FF00FF00FF00FF00FF00E6BD
      9700FEFDFC00FEFDFC00FEFDFC00FEFDFC00FEFDFC00FEFDFC00FEFDFC00E4D7
      EA00B4837800ECC79C00C5927E00FF00FF00FF00FF00FF00FF00FF00FF00E6BD
      9700FCF7F100FCF7F100FAF6F300FAF6F300FAF6F300FAF6F300FAF6F300E4D7
      EA00B4837800CC9A8100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00E6BD
      9700D8A68500D8A68500D8A68500D8A68500D8A68500D8A68500D8A68500D8A6
      8500B4837800FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
  end
  object actmngrMainActions: TActionManager
    Images = imglstActionGlyphs_16x16
    Left = 200
    Top = 104
    StyleName = 'XP Style'
    object actProceed: TAction
      Caption = 'Proceed'
      ImageIndex = 0
      OnExecute = actProceedExecute
      OnUpdate = actProceedUpdate
    end
    object actFinish: TAction
      Caption = 'Finish'
      ImageIndex = 1
      OnExecute = actFinishExecute
      OnUpdate = actFinishUpdate
    end
  end
  object imglstActionGlyphs_16x16: TImageList
    Left = 232
    Top = 104
    Bitmap = {
      494C010102000400040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000006640D0006640D0007690F0007690F0006640D0006640D000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000664
      0D0006640D000A8C170009B0190009B31A0009B31A0009B019000A9717000768
      0E0007680E0000000000000000000000000000000000B68A7F00A5787300A578
      7300A5787300A5787300A5787300A5787300A5787300A5787300A5787300A578
      7300A5787300986C670000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000007690F00097D
      14000BB41D0009B31A0009B31A0009B0190009B31A0009B31A0009B31A0009B3
      1A00097D140006640D00000000000000000000000000B68A7F00FBF6F100E6E0
      EC00F0D1A600F0D1A600F0D1A600F0D1A600EFCFA400EDCCA100EDCCA100EDCC
      A100F0D1A600986C670000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000007690F000A8C170016B3
      2D0012B327000BB41D0009B0190009B0190009B0190009B0190009B0190009B3
      1A0009B31A00097D140006650D000000000000000000B4837800FBF6F1009833
      000098330000983300009833000098330000983300009833000098330000EDCC
      A100F0D1A600986C670000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000007690F0025B5490023B5
      440016B32D000BB41D001EB13600DDF6E700ECFAF1005BCE800009B0190009B0
      190009B31A0009B31A0006650D000000000000000000B4837800FCF7F1009833
      0000FEFDFC00FEFDFC00FEFDFC008D90DF00E1C6EE00FEFDFC0098330000EECD
      A200F0D1A600986C670000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000872120020B03B002EBC5C0023B5
      44000EB421000AB41B000EB4210098E0AF00FBFDFC00F6FCF80055CC7B0009B0
      190009B0190009B31A000A97170006640D0000000000BF948400FEFCFA009833
      0000FEFDFC00FEFDFC008082EA000735FA00576CF900FEFDFC0098330000EFCF
      A400F0D1A600986C670000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000087212002CBA590033C0640027B7
      4E000EB4210009B31A000BB41D0009B019008DDDA500FBFDFC00F6FCF80055CC
      7B0009B0190009B31A0009AD190006640D0000000000BF948400FEFDFC009833
      0000E6E0EC004F64F9000735FA004F64F9000936FB00E6E0EC0098330000F0D1
      A600F0D1A600986C670000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000097D140039C36B0039C36B00CEF1
      DB00C9EFD600C9EFD600C9EFD600C0EDCF00C0EDCF00F9FDFA00FBFDFC00F0FA
      F3006DD38F0009B31A0009B31A0006650D0000000000CC9A8100FEFDFC009833
      0000576CF9001137FB00E1C6EE00F2F1F7001938FA00576CF90097330200F0D1
      A600F0D1A600986C670000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000A8C17005BCE800047C87500FBFD
      FC00FBFDFC00FBFDFC00FBFDFC00FBFDFC00FBFDFC00FBFDFC00FBFDFC00FBFD
      FC00B9EBC9000AB41B0009B31A0007690F0000000000CC9A8100FEFDFC009833
      0000ECEAF400E6E0EC00FEFDFC00FEFDFC0099A2C3000735FA00675B5900F0D1
      A600F0D1A600986C670000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000A8C17006DD38F0067D18B006DD3
      8F0075D6940075D6940075D6940067D18B008DDDA500FBFDFB00FBFDFC00A3E4
      B80027B74E0012B3270009B0190006640D0000000000DBA98400FEFDFC009833
      0000FEFDFC00FEFDFC00FEFDFC00FEFDFC00FEFDFC00576CF9000735FA00F0D1
      A600F0D1A600986C670000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000A8C170060CF8400A3E4B8003DC4
      6E0036C268003DC46E003DC46E0075D69400E6F8ED00FBFDFC0098E0AF0025B5
      490022B13D0012B327000AA3180006640D0000000000DBA98400FEFDFC009833
      000098330000983300009833000098330000983300008F330E001C35F8000735
      FA00E3BF9A00986C670000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000020B03B00C0EDCF0085DA
      A00033C0640036C2680055CC7B00F7FBF900FBFDFC0098E0AF0025B5490023B5
      440019B2310016B32D000A8C17000000000000000000DFB08C00FEFDFC00FEFD
      FC00FEFDFC00FEFDFC00FEFDFC00FCF9F500FBF6F100E6E0EC00B48378000735
      FA000735FA000735FA0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000020B03B006DD38F00D3F3
      E00085DAA00039C36B003DC46E00C0EDCF00ADE7C00033C0640029B9550027B7
      4E0023B5440019B231000A8C17000000000000000000DFB08C00FEFDFC00FEFD
      FC00FEFDFC00FEFDFC00FEFDFC00FEFDFC00FEFDFB00E4D7EA00B4837800DFB0
      8C00DBA984000735FA0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000020B03B0085DA
      A000DDF6E700ADE7C0006DD38F0055CC7B0047C875004DCA78004DCA780039C3
      6B0025B54900097D1400000000000000000000000000E6BD9700FEFDFC00FEFD
      FC00FEFDFC00FEFDFC00FEFDFC00FEFDFC00FEFDFC00E4D7EA00B4837800ECC7
      9C00C5927E000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000020B0
      3B0020B03B00ADE7C000D0F2DD00C0EDCF00ADE7C00098E0AF006DD38F002CBA
      59002CBA590000000000000000000000000000000000E6BD9700FCF7F100FCF7
      F100FAF6F300FAF6F300FAF6F300FAF6F300FAF6F300E4D7EA00B4837800CC9A
      8100000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000027B74E0020B03B0020B03B0020B03B0020B03B0023B342000000
      00000000000000000000000000000000000000000000E6BD9700D8A68500D8A6
      8500D8A68500D8A68500D8A68500D8A68500D8A68500D8A68500B48378000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF00F81FFFFF00000000E007800300000000
      C003800300000000800180030000000080018003000000000000800300000000
      0000800300000000000080030000000000008003000000000000800300000000
      000080030000000080018003000000008001800300000000C003800700000000
      E007800F00000000F81F801F0000000000000000000000000000000000000000
      000000000000}
  end
  object appevMainEvents: TApplicationEvents
    OnIdle = appevMainEventsIdle
    Left = 264
    Top = 104
  end
  object xmlDoc: TXMLDocument
    Left = 328
    Top = 104
    DOMVendorDesc = 'Open XML'
  end
end