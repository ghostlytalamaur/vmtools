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
    BevelOuter = bvNone
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
    end
    object StatusBar1: TStatusBar
      Left = 0
      Top = 282
      Width = 542
      Height = 19
      Panels = <
        item
          Text = 'Status: '
          Width = 200
        end>
    end
    object cmbFilter: TComboBox
      Left = 8
      Top = 8
      Width = 497
      Height = 21
      AutoComplete = False
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      OnChange = cmbFilterChange
      OnKeyDown = OnKeyDownEvent
    end
    object lvFiles: TListView
      Left = 8
      Top = 35
      Width = 526
      Height = 240
      Anchors = [akLeft, akTop, akRight, akBottom]
      BevelInner = bvNone
      BevelOuter = bvNone
      Columns = <
        item
          Caption = '#'
          Width = 25
        end
        item
          Caption = 'File'
          Width = 200
        end
        item
          AutoSize = True
          Caption = 'Path'
        end>
      OwnerData = True
      RowSelect = True
      TabOrder = 1
      ViewStyle = vsReport
      OnData = lvFilesData
      OnKeyDown = OnKeyDownEvent
    end
  end
end
