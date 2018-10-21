object SearchEngineQueryDlg: TSearchEngineQueryDlg
  Left = 0
  Top = 0
  ActiveControl = cmbQuery
  BorderIcons = [biSystemMenu]
  Caption = 'Search Query'
  ClientHeight = 280
  ClientWidth = 555
  Color = clBtnFace
  Constraints.MinHeight = 225
  Constraints.MinWidth = 400
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesigned
  PixelsPerInch = 96
  TextHeight = 13
  object pnlMain: TPanel
    Left = 0
    Top = 0
    Width = 555
    Height = 239
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 564
    ExplicitHeight = 248
    DesignSize = (
      555
      239)
    object lblQuery: TLabel
      Left = 8
      Top = 11
      Width = 65
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Query:'
    end
    object lblFileRegExp: TLabel
      Left = 8
      Top = 40
      Width = 65
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'File regexp:'
    end
    object cmbQuery: TComboBoxEx
      Left = 75
      Top = 9
      Width = 470
      Height = 22
      ItemsEx = <>
      StyleEx = [csExPathWordBreak]
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      ExplicitWidth = 469
    end
    inline OptionsFrame: TOptionsFrame
      Left = 8
      Top = 65
      Width = 537
      Height = 168
      Anchors = [akLeft, akTop, akRight, akBottom]
      TabOrder = 2
      ExplicitLeft = 8
      ExplicitTop = 65
      ExplicitWidth = 546
      ExplicitHeight = 177
      inherited vstOptions: TVirtualStringTree
        Width = 537
        Height = 168
        ExplicitWidth = 546
        ExplicitHeight = 177
        Columns = <
          item
            Position = 0
            Width = 333
            WideText = 'Key'
          end
          item
            Position = 1
            Width = 200
            WideText = 'Value'
          end>
      end
    end
    object cmbFileRegExp: TComboBoxEx
      Left = 75
      Top = 37
      Width = 470
      Height = 22
      ItemsEx = <>
      StyleEx = [csExPathWordBreak]
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      ExplicitWidth = 469
    end
  end
  object pnlButtons: TPanel
    Left = 0
    Top = 239
    Width = 555
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitTop = 248
    ExplicitWidth = 564
    DesignSize = (
      555
      41)
    object btnOk: TButton
      Left = 387
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Search'
      ModalResult = 1
      TabOrder = 0
      OnClick = btnOkClick
      ExplicitLeft = 396
    end
    object btnCancel: TButton
      Left = 470
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
      ExplicitLeft = 479
    end
  end
end
