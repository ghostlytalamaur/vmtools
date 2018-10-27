object SearchResultFrame: TSearchResultFrame
  Left = 0
  Top = 0
  Width = 663
  Height = 341
  DoubleBuffered = True
  ParentDoubleBuffered = False
  TabOrder = 0
  OnEnter = FrameEnter
  object vstResults: TVirtualStringTree
    Left = 0
    Top = 20
    Width = 663
    Height = 301
    Align = alClient
    BorderStyle = bsNone
    ClipboardFormats.Strings = (
      'HTML Format'
      'Unicode text')
    Colors.UnfocusedColor = clBtnShadow
    Colors.UnfocusedSelectionColor = clHotLight
    DrawSelectionMode = smBlendedRectangle
    EmptyListMessage = 'No search results...'
    Header.AutoSizeIndex = 0
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.Options = [hoColumnResize, hoDrag, hoShowSortGlyphs, hoFullRepaintOnResize, hoHeaderClickAutoSort]
    Header.SortColumn = 0
    ParentColor = True
    PopupMenu = pmTreeView
    TabOrder = 0
    TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScroll, toAutoScrollOnExpand, toAutoSort, toAutoTristateTracking, toAutoDeleteMovedNodes, toAutoChangeScale]
    TreeOptions.MiscOptions = [toAcceptOLEDrop, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning, toNodeHeightResize, toEditOnClick]
    TreeOptions.PaintOptions = [toHideFocusRect, toShowButtons, toShowDropmark, toThemeAware, toUseBlendedImages]
    TreeOptions.SelectionOptions = [toFullRowSelect]
    OnCanSplitterResizeNode = vstResultsCanSplitterResizeNode
    OnChange = vstResultsChange
    OnCompareNodes = vstResultsCompareNodes
    OnDblClick = vstResultsDblClick
    OnDrawText = vstResultsDrawText
    OnGetText = vstResultsGetText
    OnInitNode = vstResultsInitNode
    OnNodeHeightTracking = vstResultsNodeHeightTracking
    Columns = <
      item
        Position = 0
        Width = 400
        WideText = 'Text'
      end
      item
        Position = 1
        Width = 100
        WideText = 'File'
      end
      item
        Position = 2
        WideText = 'Line'
      end
      item
        Position = 3
        WideText = 'Rating'
      end
      item
        Position = 4
        WideText = 'Path'
      end>
  end
  object tbcTabs: TTabSet
    Left = 0
    Top = 0
    Width = 663
    Height = 20
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    Align = alTop
    EndMargin = 0
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    PopupMenu = pmTabs
    Style = tsModernTabs
    Tabs.Strings = (
      'Tab 1'
      'Tab 2'
      'Very Very Long Tab Name')
    TabIndex = 0
    TabPosition = tpTop
    OnChange = tbcTabsChange
  end
  object pnlStatus: TFlowPanel
    Left = 0
    Top = 321
    Width = 663
    Height = 20
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object btnShowErrors: TSpeedButton
      AlignWithMargins = True
      Left = 8
      Top = 2
      Width = 93
      Height = 16
      Margins.Left = 8
      Margins.Top = 2
      Margins.Right = 8
      Margins.Bottom = 2
      Caption = 'Show errors...'
      Visible = False
      OnClick = btnShowErrorsClick
    end
    object btnCancel: TSpeedButton
      AlignWithMargins = True
      Left = 117
      Top = 2
      Width = 93
      Height = 16
      Margins.Left = 8
      Margins.Top = 2
      Margins.Right = 8
      Margins.Bottom = 2
      Caption = '&Cancel'
      Visible = False
      OnClick = btnCancelClick
    end
    object lblStatus: TLabel
      AlignWithMargins = True
      Left = 226
      Top = 3
      Width = 31
      Height = 13
      Margins.Left = 8
      Margins.Right = 8
      Margins.Bottom = 0
      Caption = 'Status'
      Visible = False
    end
  end
  object pmTabs: TPopupMenu
    Left = 24
    Top = 144
    object miCloseTab: TMenuItem
      Action = actCloseTab
    end
  end
  object actlst: TActionList
    Left = 272
    Top = 176
    object actCloseTab: TAction
      Caption = 'Close Tab'
      ShortCut = 16471
      OnExecute = actCloseTabExecute
    end
  end
  object pmTreeView: TPopupMenu
    OnPopup = pmTreeViewPopup
    Left = 160
    Top = 64
    object miShowHeader: TMenuItem
      Caption = 'Show Header'
      OnClick = miShowHeaderClick
    end
  end
end
