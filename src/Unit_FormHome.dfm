object FormHome: TFormHome
  Left = 0
  Top = 0
  Caption = 'Agenda de Contatos'
  ClientHeight = 460
  ClientWidth = 780
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  PixelsPerInch = 96
  TextHeight = 15
  object EditPesquisa: TEdit
    Left = 20
    Top = 15
    Width = 350
    Height = 23
    TabOrder = 0
    TextHint = 'Pesquisar por nome ou telefone...'
  end
  object BtnPesquisar: TButton
    Left = 380
    Top = 13
    Width = 90
    Height = 28
    Caption = 'Pesquisar'
    TabOrder = 1
    OnClick = BtnPesquisarClick
  end
  object BtnNovo: TButton
    Left = 640
    Top = 13
    Width = 120
    Height = 28
    Caption = 'Novo Contato'
    TabOrder = 2
    OnClick = BtnNovoClick
  end
  object DBGridContatos: TDBGrid
    Left = 20
    Top = 50
    Width = 740
    Height = 380
    TabOrder = 3
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -12
    TitleFont.Name = 'Segoe UI'
    TitleFont.Style = []
    ReadOnly = True
    Options = [dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgRowSelect]
  end
end
