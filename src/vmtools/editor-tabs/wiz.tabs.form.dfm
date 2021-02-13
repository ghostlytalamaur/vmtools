object TabsListForm: TTabsListForm
  Left = 0
  Top = 0
  BorderStyle = bsNone
  Caption = 'TabsListForm'
  ClientHeight = 247
  ClientWidth = 693
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  KeyPreview = True
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object lvFiles: TListView
    AlignWithMargins = True
    Left = 4
    Top = 4
    Width = 685
    Height = 239
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alClient
    BevelInner = bvNone
    BevelOuter = bvNone
    BorderStyle = bsNone
    Columns = <
      item
        Caption = 'File'
        Width = 256
      end
      item
        AutoSize = True
        Caption = 'Path'
      end>
    OwnerData = True
    RowSelect = True
    ShowColumnHeaders = False
    TabOrder = 0
    ViewStyle = vsReport
    OnData = lvFilesData
    OnKeyDown = lvFilesKeyDown
    OnKeyUp = lvFilesKeyUp
  end
end
