object OptForm: TOptForm
  Left = 0
  Top = 0
  ActiveControl = btnSearch
  AlphaBlend = True
  Caption = 'OptForm'
  ClientHeight = 656
  ClientWidth = 833
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
    Top = 0
    Width = 833
    Height = 57
    Align = alTop
    TabOrder = 0
    object btnTemplates: TBitBtn
      Left = 248
      Top = 11
      Width = 75
      Height = 25
      Caption = 'Templates...'
      TabOrder = 0
      OnClick = btnTemplatesClick
    end
    object btnOk: TBitBtn
      Left = 5
      Top = 11
      Width = 75
      Height = 25
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 1
    end
    object btnCancel: TBitBtn
      Left = 86
      Top = 11
      Width = 75
      Height = 25
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 2
    end
    object btnSearch: TBitBtn
      Left = 167
      Top = 11
      Width = 75
      Height = 25
      Caption = 'Search...'
      TabOrder = 3
      OnClick = btnSearchClick
    end
    object btnTestAction: TButton
      Left = 336
      Top = 11
      Width = 75
      Height = 25
      Caption = 'btnTestAction'
      TabOrder = 4
      OnClick = btnTestActionClick
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 57
    Width = 833
    Height = 599
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 1
    object TabSheet1: TTabSheet
      Caption = 'Options'
      object pnlOptions: TPanel
        Left = 0
        Top = 0
        Width = 825
        Height = 571
        Align = alClient
        TabOrder = 0
        inline OptionsFrame1: TOptionsFrame
          Left = 1
          Top = 1
          Width = 737
          Height = 569
          Align = alClient
          TabOrder = 0
          ExplicitLeft = 1
          ExplicitTop = 1
          ExplicitWidth = 737
          ExplicitHeight = 569
          inherited vstOptions: TVirtualStringTree
            Width = 737
            Height = 569
            ExplicitWidth = 737
            ExplicitHeight = 569
            Columns = <
              item
                Position = 0
                Width = 537
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
          Left = 738
          Top = 1
          Width = 86
          Height = 569
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
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object pnlFiles: TPanel
        Left = 0
        Top = 0
        Width = 825
        Height = 571
        Align = alClient
        TabOrder = 0
        inline OpenFileFrame1: TOpenFileFrame
          Left = 1
          Top = 1
          Width = 823
          Height = 569
          Align = alClient
          TabOrder = 0
          ExplicitLeft = 1
          ExplicitTop = 1
          ExplicitWidth = 823
          ExplicitHeight = 569
          inherited Panel1: TPanel
            Width = 823
            Height = 569
            ExplicitWidth = 823
            ExplicitHeight = 569
            inherited btnRebuild: TSpeedButton
              Left = 792
              ExplicitLeft = 794
            end
            inherited vstFiles: TVirtualStringTree
              Width = 807
              Height = 508
              ExplicitWidth = 807
              ExplicitHeight = 508
            end
            inherited StatusBar1: TStatusBar
              Top = 549
              Width = 821
              ExplicitTop = 549
              ExplicitWidth = 821
            end
            inherited cmbFilter: TComboBoxEx
              Width = 778
              ExplicitWidth = 778
            end
          end
        end
      end
    end
  end
end
