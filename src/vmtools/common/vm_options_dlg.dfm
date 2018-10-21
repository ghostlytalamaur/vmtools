object OptionsDlg: TOptionsDlg
  Left = 0
  Top = 0
  Caption = 'Options'
  ClientHeight = 560
  ClientWidth = 800
  Color = clBtnFace
  Constraints.MinHeight = 560
  Constraints.MinWidth = 800
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
    Top = 519
    Width = 800
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitTop = 256
    ExplicitWidth = 456
    DesignSize = (
      800
      41)
    object btnOk: TButton
      Left = 639
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Ok'
      ModalResult = 1
      TabOrder = 0
      ExplicitLeft = 295
    end
    object btnCancel: TButton
      Left = 721
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
      ExplicitLeft = 377
    end
  end
  object pnlMain: TPanel
    AlignWithMargins = True
    Left = 2
    Top = 4
    Width = 794
    Height = 511
    Margins.Left = 2
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object pnlList: TPanel
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 197
      Height = 503
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Align = alLeft
      BevelOuter = bvNone
      Caption = 'pnlList'
      TabOrder = 0
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitHeight = 494
      object lstFrames: TListBox
        Left = 0
        Top = 0
        Width = 197
        Height = 503
        Align = alClient
        ItemHeight = 13
        TabOrder = 0
        OnClick = lstFramesClick
        ExplicitLeft = -20
        ExplicitTop = -52
        ExplicitWidth = 185
        ExplicitHeight = 261
      end
    end
    object pnlFrame: TPanel
      AlignWithMargins = True
      Left = 209
      Top = 4
      Width = 581
      Height = 503
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      ExplicitWidth = 600
      ExplicitHeight = 502
    end
  end
end
