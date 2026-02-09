object FormContato: TFormContato
  Left = 0
  Top = 0
  Caption = 'Contato'
  ClientHeight = 440
  ClientWidth = 650
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  PixelsPerInch = 96
  TextHeight = 15
  object LabelNome: TLabel
    Left = 20
    Top = 15
    Width = 34
    Height = 15
    Caption = 'Nome:'
  end
  object DBEditNome: TDBEdit
    Left = 20
    Top = 33
    Width = 320
    Height = 23
    DataField = 'NOME'
    TabOrder = 0
  end
  object LabelIdade: TLabel
    Left = 360
    Top = 15
    Width = 33
    Height = 15
    Caption = 'Idade:'
  end
  object DBEditIdade: TDBEdit
    Left = 360
    Top = 33
    Width = 70
    Height = 23
    DataField = 'IDADE'
    TabOrder = 1
  end
  object LabelTelefone: TLabel
    Left = 20
    Top = 70
    Width = 52
    Height = 15
    Caption = 'Telefone:'
  end
  object EditTelefone: TEdit
    Left = 20
    Top = 88
    Width = 200
    Height = 23
    TabOrder = 2
    TextHint = 'Digite o numero...'
  end
  object BtnAdicionarTelefone: TButton
    Left = 230
    Top = 86
    Width = 90
    Height = 28
    Caption = 'Adicionar'
    TabOrder = 3
    OnClick = BtnAdicionarTelefoneClick
  end
  object BtnRemoverTelefone: TButton
    Left = 330
    Top = 86
    Width = 30
    Height = 28
    Caption = '-'
    TabOrder = 4
    Visible = False
    OnClick = BtnRemoverTelefoneClick
  end
  object DBGridTelefones: TDBGrid
    Left = 20
    Top = 120
    Width = 350
    Height = 150
    TabOrder = 5
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -12
    TitleFont.Name = 'Segoe UI'
    TitleFont.Style = []
    ReadOnly = True
    Options = [dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgRowSelect]
  end
  object BtnSalvar: TButton
    Left = 20
    Top = 290
    Width = 90
    Height = 30
    Caption = 'Salvar'
    TabOrder = 6
    OnClick = BtnSalvarClick
  end
  object BtnCancelar: TButton
    Left = 120
    Top = 290
    Width = 90
    Height = 30
    Caption = 'Voltar'
    TabOrder = 7
    OnClick = BtnCancelarClick
  end
  object BtnExcluirContato: TButton
    Left = 280
    Top = 290
    Width = 120
    Height = 30
    Caption = 'Excluir Contato'
    TabOrder = 8
    OnClick = BtnExcluirContatoClick
  end
end
