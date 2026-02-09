object FormServer: TFormServer
  Left = 0
  Top = 0
  Caption = 'Agenda de Contatos - Servidor D2Bridge'
  ClientHeight = 340
  ClientWidth = 500
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 15
  object PanelTop: TPanel
    Left = 0
    Top = 0
    Width = 500
    Height = 90
    Align = alTop
    TabOrder = 0
    object LabelTitle: TLabel
      Left = 16
      Top = 10
      Width = 200
      Height = 20
      Caption = 'Agenda de Contatos - D2Bridge'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object LabelPort: TLabel
      Left = 16
      Top = 45
      Width = 33
      Height = 15
      Caption = 'Porta:'
    end
    object EditPort: TEdit
      Left = 55
      Top = 42
      Width = 60
      Height = 23
      TabOrder = 0
      Text = '8080'
    end
    object BtnStart: TButton
      Left = 130
      Top = 40
      Width = 80
      Height = 28
      Caption = 'Iniciar'
      TabOrder = 1
      OnClick = BtnStartClick
    end
    object BtnStop: TButton
      Left = 220
      Top = 40
      Width = 80
      Height = 28
      Caption = 'Parar'
      Enabled = False
      TabOrder = 2
      OnClick = BtnStopClick
    end
    object LabelStatus: TLabel
      Left = 320
      Top = 45
      Width = 38
      Height = 15
      Caption = 'Parado'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
    end
  end
  object MemoLog: TMemo
    Left = 0
    Top = 90
    Width = 500
    Height = 250
    Align = alClient
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 1
  end
end
