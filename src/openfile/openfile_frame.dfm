object OpenFileFrame: TOpenFileFrame
  Left = 0
  Top = 0
  Width = 542
  Height = 301
  TabOrder = 0
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 542
    Height = 301
    Align = alClient
    TabOrder = 0
    DesignSize = (
      542
      301)
    object btnRebuild: TSpeedButton
      Left = 511
      Top = 7
      Width = 23
      Height = 22
      Anchors = [akTop, akRight]
      Caption = 'R'
      Flat = True
      OnClick = btnRebuildClick
      ExplicitLeft = 463
    end
    object vstFiles: TVirtualStringTree
      Left = 8
      Top = 35
      Width = 526
      Height = 240
      Anchors = [akLeft, akTop, akRight, akBottom]
      Header.AutoSizeIndex = 0
      Header.Font.Charset = DEFAULT_CHARSET
      Header.Font.Color = clWindowText
      Header.Font.Height = -11
      Header.Font.Name = 'Tahoma'
      Header.Font.Style = []
      Header.Options = [hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
      TabOrder = 0
      OnFreeNode = vstFilesFreeNode
      OnGetText = vstFilesGetText
      OnInitNode = vstFilesInitNode
      OnNodeDblClick = vstFilesNodeDblClick
      Columns = <
        item
          MinWidth = 250
          Position = 0
          Width = 350
          WideText = 'File name'
        end
        item
          Position = 1
          WideText = 'File path'
        end>
    end
    object StatusBar1: TStatusBar
      Left = 1
      Top = 281
      Width = 540
      Height = 19
      Panels = <
        item
          Text = 'Status: '
          Width = 200
        end>
    end
    object cmbFilter: TComboBoxEx
      Left = 8
      Top = 7
      Width = 497
      Height = 22
      AutoCompleteOptions = []
      ItemsEx = <>
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 2
      Text = 'cmbFilter'
      OnChange = cmbFilterChange
    end
  end
end
