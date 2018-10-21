object ActionManagerFrame: TActionManagerFrame
  Left = 0
  Top = 0
  Width = 674
  Height = 476
  TabOrder = 0
  object pnlAssign: TPanel
    Left = 0
    Top = 0
    Width = 674
    Height = 476
    Align = alClient
    BevelKind = bkFlat
    BevelOuter = bvNone
    Caption = 'pnlAssign'
    TabOrder = 0
    DesignSize = (
      670
      472)
    object lblError: TLabel
      Left = 7
      Top = 447
      Width = 379
      Height = 19
      Anchors = [akLeft, akRight, akBottom]
      AutoSize = False
      Caption = 'Short Alt + Shift + E already used by action "Focus editor"'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object lvShortCuts: TListView
      Left = 7
      Top = 7
      Width = 657
      Height = 429
      Anchors = [akLeft, akTop, akRight, akBottom]
      Columns = <
        item
          AutoSize = True
          Caption = 'Action'
          MinWidth = 100
        end
        item
          AutoSize = True
          Caption = 'ShortCut'
        end>
      Groups = <
        item
          GroupID = 0
          State = [lgsNormal]
          HeaderAlign = taLeftJustify
          FooterAlign = taLeftJustify
          TitleImage = -1
        end>
      Items.ItemData = {
        05B20000000200000000000000FFFFFFFFFFFFFFFF01000000FFFFFFFF000000
        000C46006F00630075007300200065006400690074006F00720008530068006F
        0072007400430075007400A829631C00000000FFFFFFFFFFFFFFFF01000000FF
        FFFFFF000000001A530068006F00770020005300650061007200630068002000
        52006500730075006C007400730020004400690061006C006F0067000A530068
        006F007200740043007500740020003200B02E631CFFFFFFFF}
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
      OnChange = lvShortCutsChange
    end
    object hkAssign: THotKey
      Left = 392
      Top = 445
      Width = 189
      Height = 19
      Anchors = [akRight, akBottom]
      HotKey = 57467
      Modifiers = [hkShift, hkCtrl, hkAlt]
      TabOrder = 1
      OnChange = hkAssignChange
    end
    object btnAssign: TButton
      Left = 589
      Top = 442
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'Assign'
      TabOrder = 2
      OnClick = btnAssignClick
    end
  end
end
