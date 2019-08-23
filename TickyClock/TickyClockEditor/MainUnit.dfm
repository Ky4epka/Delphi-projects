object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 644
  ClientWidth = 638
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 440
    Top = 0
    Width = 198
    Height = 644
    Align = alRight
    TabOrder = 0
    ExplicitHeight = 608
    object GroupBox1: TGroupBox
      Left = 1
      Top = 1
      Width = 196
      Height = 304
      Align = alTop
      Caption = #1064#1072#1073#1083#1086#1085
      TabOrder = 0
      object Label2: TLabel
        Left = 16
        Top = 69
        Width = 37
        Height = 13
        Caption = #1057#1080#1084#1074#1086#1083
      end
      object Label3: TLabel
        Left = 16
        Top = 117
        Width = 40
        Height = 13
        Caption = #1064#1080#1088#1080#1085#1072
      end
      object Label4: TLabel
        Left = 16
        Top = 165
        Width = 37
        Height = 13
        Caption = #1042#1099#1089#1086#1090#1072
      end
      object Label1: TLabel
        Left = 16
        Top = 22
        Width = 56
        Height = 13
        Caption = #1064#1072#1073#1083#1086#1085' '#8470
      end
      object TemplateCharEdit: TEdit
        Left = 16
        Top = 88
        Width = 121
        Height = 21
        TabOrder = 0
      end
      object NewTemplateBtn: TButton
        Left = 16
        Top = 267
        Width = 153
        Height = 25
        Caption = #1053#1086#1074#1099#1081' '#1096#1072#1073#1083#1086#1085
        TabOrder = 1
        OnClick = NewTemplateBtnClick
      end
      object TemplateWidthEdit: TSpinEdit
        Left = 16
        Top = 136
        Width = 121
        Height = 22
        MaxValue = 360
        MinValue = 0
        TabOrder = 2
        Value = 0
      end
      object TemplateHeightEdit: TSpinEdit
        Left = 16
        Top = 184
        Width = 121
        Height = 22
        MaxValue = 360
        MinValue = 0
        TabOrder = 3
        Value = 0
      end
      object TemplateEdit: TSpinEdit
        Left = 16
        Top = 41
        Width = 121
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 4
        Value = 0
        OnChange = TemplateEditChange
      end
      object ApplyTemplateBtn: TButton
        Left = 16
        Top = 228
        Width = 153
        Height = 25
        Caption = #1055#1088#1080#1084#1077#1085#1080#1090#1100
        TabOrder = 5
        OnClick = ApplyTemplateBtnClick
      end
    end
    object LoadTemplatesBtn: TButton
      Left = 1
      Top = 591
      Width = 196
      Height = 25
      Align = alBottom
      Caption = #1047#1072#1075#1088#1091#1079#1080#1090#1100' '#1096#1072#1073#1083#1086#1085#1099
      TabOrder = 1
      OnClick = LoadTemplatesBtnClick
      ExplicitLeft = 24
      ExplicitTop = 576
      ExplicitWidth = 145
    end
    object SaveTemplatesBtn: TButton
      Left = 1
      Top = 616
      Width = 196
      Height = 27
      Align = alBottom
      Caption = #1057#1086#1093#1088#1072#1085#1080#1090#1100' '#1096#1072#1073#1083#1086#1085#1099
      ModalResult = 1
      TabOrder = 2
      OnClick = SaveTemplatesBtnClick
      ExplicitLeft = 6
      ExplicitWidth = 191
    end
    object GroupBox2: TGroupBox
      Left = 1
      Top = 305
      Width = 196
      Height = 215
      Align = alTop
      Caption = #1069#1083#1077#1084#1077#1085#1090
      TabOrder = 3
      ExplicitTop = 376
      object Label6: TLabel
        Left = 16
        Top = 61
        Width = 88
        Height = 13
        Caption = #1041#1086#1083#1100#1096#1072#1103' '#1089#1090#1088#1077#1083#1082#1072
      end
      object Label7: TLabel
        Left = 16
        Top = 109
        Width = 76
        Height = 13
        Caption = #1052#1072#1083#1072#1103' '#1089#1090#1088#1077#1083#1082#1072
      end
      object Label5: TLabel
        Left = 14
        Top = 18
        Width = 59
        Height = 13
        Caption = #1069#1083#1077#1084#1077#1085#1090' '#8470
      end
      object BigEdit: TSpinEdit
        Left = 16
        Top = 80
        Width = 121
        Height = 22
        MaxValue = 360
        MinValue = 0
        TabOrder = 0
        Value = 0
      end
      object SmallEdit: TSpinEdit
        Left = 16
        Top = 128
        Width = 121
        Height = 22
        MaxValue = 360
        MinValue = 0
        TabOrder = 1
        Value = 0
      end
      object SaveToTemplateBtn: TButton
        Left = 16
        Top = 179
        Width = 153
        Height = 25
        Caption = #1057#1086#1093#1088#1072#1085#1080#1090#1100' '#1074' '#1096#1072#1073#1083#1086#1085
        TabOrder = 2
        OnClick = SaveToTemplateBtnClick
      end
      object ElementEdit: TSpinEdit
        Left = 16
        Top = 37
        Width = 121
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 3
        Value = 0
        OnChange = ElementEditChange
      end
    end
  end
  object MainLayer: TPanel
    Left = 0
    Top = 0
    Width = 440
    Height = 644
    Align = alClient
    TabOrder = 1
    ExplicitLeft = 112
    ExplicitTop = 272
    ExplicitWidth = 185
    ExplicitHeight = 41
    object TickyPanel: TPanel
      Left = 0
      Top = 0
      Width = 160
      Height = 220
      Align = alCustom
      BevelKind = bkSoft
      TabOrder = 0
    end
    object Panel2: TPanel
      Left = 159
      Top = 0
      Width = 160
      Height = 220
      Align = alCustom
      BevelKind = bkSoft
      TabOrder = 1
    end
  end
  object TickyTimer: TTimer
    Enabled = False
    Interval = 20
    OnTimer = TickyTimerTimer
    Left = 40
    Top = 288
  end
  object OpenTemplateDialog: TOpenDialog
    Filter = 'Templates file (*.tmps)|*.tmps'
    FilterIndex = 0
    Left = 32
    Top = 344
  end
  object SaveTemplateDialog: TSaveDialog
    FileName = 'NewTemplates'
    Filter = 'Templates file (*.tmps)|*.tmps'
    Left = 32
    Top = 400
  end
end
