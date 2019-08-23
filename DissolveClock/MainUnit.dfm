object Form1: TForm1
  Left = 265
  Top = 93
  BorderStyle = bsNone
  Caption = 'Form1'
  ClientHeight = 185
  ClientWidth = 190
  Color = clWhite
  TransparentColorValue = clWhite
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesigned
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object clickClose: TPanel
    Left = 96
    Top = 112
    Width = 73
    Height = 33
    BevelOuter = bvNone
    Caption = 'Close X'
    Color = clWhite
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentBackground = False
    ParentFont = False
    TabOrder = 0
    object clickArea: TShape
      Left = 0
      Top = 0
      Width = 73
      Height = 33
      Align = alClient
      Brush.Style = bsClear
      OnMouseDown = clickAreaMouseDown
      OnMouseEnter = clickAreaMouseEnter
      OnMouseLeave = clickAreaMouseLeave
      OnMouseUp = clickAreaMouseUp
      ExplicitLeft = 24
      ExplicitTop = 8
      ExplicitWidth = 65
      ExplicitHeight = 65
    end
  end
  object CloseBtnTimer: TTimer
    Interval = 100
    OnTimer = CloseBtnTimerTimer
    Left = 608
    Top = 16
  end
  object PosTimer: TTimer
    Interval = 1
    OnTimer = PosTimerTimer
    Left = 152
    Top = 32
  end
  object TrayIcon: TTrayIcon
    Visible = True
    Left = 152
    Top = 152
  end
  object PopupMenu: TPopupMenu
    Left = 120
    Top = 152
    object Exit1: TMenuItem
      Caption = 'Exit'
      OnClick = Exit1Click
    end
  end
end
