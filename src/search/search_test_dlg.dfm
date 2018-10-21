object SearchTestDlg: TSearchTestDlg
  Left = 0
  Top = 0
  ActiveControl = btnSearch
  Caption = 'Search Test'
  ClientHeight = 489
  ClientWidth = 962
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  inline SearchResultFrame1: TSearchResultFrame
    Left = 0
    Top = 41
    Width = 962
    Height = 448
    Align = alClient
    DoubleBuffered = True
    ParentDoubleBuffered = False
    TabOrder = 0
    ExplicitTop = 41
    ExplicitWidth = 962
    ExplicitHeight = 448
    inherited vstResults: TVirtualStringTree
      Width = 962
      Height = 409
      ExplicitWidth = 962
      ExplicitHeight = 409
    end
    inherited tbcTabs: TTabSet
      Width = 962
      ExplicitWidth = 962
    end
    inherited StatusBar1: TStatusBar
      Top = 429
      Width = 962
      ExplicitTop = 429
      ExplicitWidth = 962
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 962
    Height = 41
    Align = alTop
    TabOrder = 1
    object btnSearch: TButton
      Left = 8
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Search'
      TabOrder = 0
      OnClick = btnSearchClick
    end
    object btnPrev: TButton
      Left = 89
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Prev'
      TabOrder = 1
      OnClick = btnPrevClick
    end
    object btnNext: TButton
      Left = 170
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Next'
      TabOrder = 2
      OnClick = btnNextClick
    end
  end
end
