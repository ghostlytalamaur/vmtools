object OptForm: TOptForm
  Left = 0
  Top = 0
  AlphaBlend = True
  Caption = 'OptForm'
  ClientHeight = 656
  ClientWidth = 964
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnlTop: TPanel
    Left = 0
    Top = 616
    Width = 964
    Height = 40
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      964
      40)
    object btnOk: TBitBtn
      AlignWithMargins = True
      Left = 800
      Top = 6
      Width = 75
      Height = 25
      Margins.Left = 8
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 8
      Anchors = [akTop, akRight]
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 0
    end
    object btnCancel: TBitBtn
      AlignWithMargins = True
      Left = 885
      Top = 6
      Width = 75
      Height = 25
      Margins.Left = 8
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 8
      Anchors = [akTop, akRight]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 1
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 964
    Height = 616
    ActivePage = tsSearch
    Align = alClient
    TabOrder = 1
    object TabSheet1: TTabSheet
      Caption = 'Options'
      object pnlOptions: TPanel
        Left = 0
        Top = 0
        Width = 956
        Height = 588
        Align = alClient
        TabOrder = 0
        inline OptionsFrame1: TOptionsFrame
          Left = 1
          Top = 1
          Width = 868
          Height = 586
          Align = alClient
          TabOrder = 0
          ExplicitLeft = 1
          ExplicitTop = 1
          ExplicitWidth = 868
          ExplicitHeight = 586
          inherited vstOptions: TVirtualStringTree
            Width = 868
            Height = 586
            ExplicitWidth = 868
            ExplicitHeight = 586
            Columns = <
              item
                Position = 0
                Width = 664
                WideText = 'Key'
              end
              item
                Position = 1
                Width = 200
                WideText = 'Value'
              end>
          end
        end
        object Panel1: TPanel
          Left = 869
          Top = 1
          Width = 86
          Height = 586
          Align = alRight
          TabOrder = 1
          object btnDefaultOptions: TButton
            Left = 6
            Top = 45
            Width = 75
            Height = 25
            Caption = 'Set defaults'
            TabOrder = 0
            OnClick = btnDefaultOptionsClick
          end
          object btnApplyOptions: TButton
            Left = 6
            Top = 14
            Width = 75
            Height = 25
            Caption = 'Apply'
            TabOrder = 1
            OnClick = btnApplyOptionsClick
          end
        end
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'OpenFile'
      ImageIndex = 1
      object pnlFiles: TPanel
        Left = 0
        Top = 0
        Width = 956
        Height = 588
        Align = alClient
        TabOrder = 0
        inline OpenFileFrame1: TOpenFileFrame
          Left = 1
          Top = 1
          Width = 954
          Height = 586
          Align = alClient
          TabOrder = 0
          ExplicitLeft = 1
          ExplicitTop = 1
          ExplicitWidth = 954
          ExplicitHeight = 586
          inherited Panel1: TPanel
            Width = 954
            Height = 586
            ExplicitWidth = 954
            ExplicitHeight = 586
            inherited btnRebuild: TSpeedButton
              Left = 923
              ExplicitLeft = 794
            end
            inherited vstFiles: TVirtualStringTree
              Width = 938
              Height = 525
              ExplicitWidth = 938
              ExplicitHeight = 525
            end
            inherited StatusBar1: TStatusBar
              Top = 566
              Width = 952
              ExplicitTop = 566
              ExplicitWidth = 952
            end
            inherited cmbFilter: TComboBoxEx
              Width = 909
              ExplicitWidth = 909
            end
          end
        end
      end
    end
    object tsSearch: TTabSheet
      Caption = 'Search'
      ImageIndex = 2
      object pnlSearch: TPanel
        Left = 0
        Top = 0
        Width = 956
        Height = 588
        Align = alClient
        Caption = 'pnlSearch'
        TabOrder = 0
        inline SearchResultFrame1: TSearchResultFrame
          Left = 1
          Top = 1
          Width = 843
          Height = 586
          Align = alClient
          DoubleBuffered = True
          ParentDoubleBuffered = False
          TabOrder = 0
          ExplicitLeft = 1
          ExplicitTop = 1
          ExplicitWidth = 843
          ExplicitHeight = 586
          inherited vstResults: TVirtualStringTree
            Width = 843
            Height = 547
            ExplicitWidth = 843
            ExplicitHeight = 547
          end
          inherited tbcTabs: TTabSet
            Width = 843
            ExplicitWidth = 843
          end
          inherited pnlStatus: TFlowPanel
            Top = 567
            Width = 843
            ExplicitTop = 567
            ExplicitWidth = 843
          end
        end
        object pnlSearchRight: TPanel
          Left = 844
          Top = 1
          Width = 111
          Height = 586
          Align = alRight
          TabOrder = 1
          object btnSearch: TButton
            Left = 20
            Top = 7
            Width = 75
            Height = 25
            Caption = '&Search...'
            TabOrder = 0
            OnClick = btnSearchClick
          end
        end
      end
    end
  end
end
