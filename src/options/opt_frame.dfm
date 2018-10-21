object OptionsFrame: TOptionsFrame
  Left = 0
  Top = 0
  Width = 458
  Height = 337
  TabOrder = 0
  object vstOptions: TVirtualStringTree
    Left = 0
    Top = 0
    Width = 458
    Height = 337
    Align = alClient
    Header.AutoSizeIndex = 0
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
    HintMode = hmHint
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoSort, toAutoTristateTracking, toAutoDeleteMovedNodes, toAutoChangeScale]
    TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toWheelPanning, toEditOnClick, toEditOnDblClick]
    TreeOptions.PaintOptions = [toHideFocusRect, toShowButtons, toShowDropmark, toShowHorzGridLines, toShowRoot, toShowTreeLines, toThemeAware, toUseBlendedImages, toFullVertGridLines]
    TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect]
    OnChecked = vstOptionsChecked
    OnContextPopup = vstOptionsContextPopup
    OnCreateEditor = vstOptionsCreateEditor
    OnEditing = vstOptionsEditing
    OnFreeNode = vstOptionsFreeNode
    OnGetText = vstOptionsGetText
    OnPaintText = vstOptionsPaintText
    OnInitNode = vstOptionsInitNode
    Columns = <
      item
        Position = 0
        Width = 254
        WideText = 'Key'
      end
      item
        Position = 1
        Width = 200
        WideText = 'Value'
      end>
  end
  object pmContextMenu: TPopupMenu
    Left = 216
    Top = 176
    object mnuSetDefault: TMenuItem
      Caption = 'Set default'
      OnClick = mnuSetDefaultClick
    end
  end
end
