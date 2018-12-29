object CreateFileDlg: TCreateFileDlg
  Left = 0
  Top = 0
  Caption = 'Create File'
  ClientHeight = 497
  ClientWidth = 789
  Color = clBtnFace
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
    Top = 456
    Width = 789
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitLeft = 480
    ExplicitTop = 360
    ExplicitWidth = 185
    DesignSize = (
      789
      41)
    object btnOk: TButton
      Left = 626
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Ok'
      ModalResult = 1
      TabOrder = 0
      OnClick = btnOkClick
      ExplicitLeft = 370
    end
    object btnCancel: TButton
      Left = 707
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
      ExplicitLeft = 451
    end
  end
  object pnlMain: TPanel
    Left = 0
    Top = 0
    Width = 789
    Height = 456
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitWidth = 533
    ExplicitHeight = 73
    DesignSize = (
      789
      456)
    object edtFileName: TEdit
      Left = 8
      Top = 8
      Width = 774
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      OnChange = DoEnableControls
      ExplicitWidth = 518
    end
    object lstPaths: TListBox
      Left = 232
      Top = 35
      Width = 550
      Height = 415
      Anchors = [akLeft, akTop, akRight, akBottom]
      ItemHeight = 13
      TabOrder = 2
      OnClick = DoEnableControls
      OnEnter = DoEnableControls
      OnKeyPress = lstTemplatesKeyPress
      ExplicitWidth = 256
      ExplicitHeight = 367
    end
    object lstTemplates: TListBox
      Left = 8
      Top = 35
      Width = 218
      Height = 415
      Anchors = [akLeft, akTop, akBottom]
      ItemHeight = 13
      TabOrder = 1
      OnClick = DoEnableControls
      OnEnter = DoEnableControls
      OnKeyPress = lstTemplatesKeyPress
    end
  end
end
