object ProgressDlg: TProgressDlg
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Progress'
  ClientHeight = 97
  ClientWidth = 295
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnHide = FormHide
  DesignSize = (
    295
    97)
  PixelsPerInch = 96
  TextHeight = 13
  object pnlMain: TPanel
    Left = 0
    Top = 0
    Width = 295
    Height = 64
    Anchors = [akLeft, akTop, akRight]
    AutoSize = True
    TabOrder = 0
    DesignSize = (
      295
      64)
    object lblInfo: TLabel
      AlignWithMargins = True
      Left = 9
      Top = 9
      Width = 277
      Height = 13
      Margins.Left = 8
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 8
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
    end
    object pbBar: TProgressBar
      AlignWithMargins = True
      Left = 9
      Top = 38
      Width = 277
      Height = 17
      Margins.Left = 8
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 8
      Max = 0
      Smooth = True
      Step = 1
      TabOrder = 0
      Visible = False
    end
  end
  object pnlButtons: TPanel
    Left = 0
    Top = 56
    Width = 295
    Height = 41
    Anchors = [akLeft, akRight, akBottom]
    BevelOuter = bvNone
    Constraints.MaxHeight = 41
    TabOrder = 1
    object btnCancel: TButton
      Left = 211
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Cancel'
      TabOrder = 0
      OnClick = btnCancelClick
    end
  end
end
