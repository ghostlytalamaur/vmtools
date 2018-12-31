object CreateFileDlg: TCreateFileDlg
  Left = 0
  Top = 0
  Caption = 'Create File'
  ClientHeight = 480
  ClientWidth = 640
  Color = clBtnFace
  Constraints.MinHeight = 480
  Constraints.MinWidth = 640
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object pnlButtons: TPanel
    Left = 0
    Top = 447
    Width = 640
    Height = 33
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      640
      33)
    object btnOk: TButton
      Left = 474
      Top = 0
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Ok'
      ModalResult = 1
      TabOrder = 0
      OnClick = btnOkClick
    end
    object btnCancel: TButton
      Left = 557
      Top = 0
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object pnlMain: TPanel
    Left = 0
    Top = 0
    Width = 640
    Height = 447
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      640
      447)
    object lblTemplates: TLabel
      Left = 8
      Top = 53
      Width = 218
      Height = 16
      AutoSize = False
      Caption = 'Templates'
    end
    object lblPaths: TLabel
      Left = 234
      Top = 55
      Width = 218
      Height = 16
      AutoSize = False
      Caption = 'Search paths'
    end
    object lblFileName: TLabel
      Left = 8
      Top = 8
      Width = 218
      Height = 16
      AutoSize = False
      Caption = 'File name'
    end
    object edtFileName: TEdit
      Left = 8
      Top = 26
      Width = 624
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      OnChange = DoEnableControls
    end
    object vstTemplates: TVirtualStringTree
      Left = 8
      Top = 71
      Width = 218
      Height = 368
      Anchors = [akLeft, akTop, akBottom]
      EmptyListMessage = 'Templates not found'
      Header.AutoSizeIndex = 0
      Header.Font.Charset = DEFAULT_CHARSET
      Header.Font.Color = clWindowText
      Header.Font.Height = -11
      Header.Font.Name = 'Tahoma'
      Header.Font.Style = []
      IncrementalSearch = isAll
      IncrementalSearchStart = ssAlwaysStartOver
      TabOrder = 1
      OnFocusChanged = vstTemplatesFocusChanged
      OnGetText = vstTemplatesGetText
      OnIncrementalSearch = vstTemplatesIncrementalSearch
      Columns = <
        item
          Position = 0
          WideText = 'Template'
        end>
    end
    object vstPaths: TVirtualStringTree
      Left = 234
      Top = 71
      Width = 398
      Height = 368
      Anchors = [akLeft, akTop, akRight, akBottom]
      Header.AutoSizeIndex = 0
      Header.Font.Charset = DEFAULT_CHARSET
      Header.Font.Color = clWindowText
      Header.Font.Height = -11
      Header.Font.Name = 'Tahoma'
      Header.Font.Style = []
      IncrementalSearch = isAll
      IncrementalSearchStart = ssAlwaysStartOver
      TabOrder = 2
      OnFocusChanged = vstTemplatesFocusChanged
      OnGetText = vstPathsGetText
      OnIncrementalSearch = vstPathsIncrementalSearch
      Columns = <
        item
          Position = 0
          WideText = 'Path'
        end>
    end
  end
end
