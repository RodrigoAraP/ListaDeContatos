object DM: TDM
  OnCreate = DataModuleCreate
  Height = 480
  Width = 640
  object FDConnection: TFDConnection
    LoginPrompt = False
    Left = 56
    Top = 24
  end
  object FDGUIxWaitCursor: TFDGUIxWaitCursor
    Provider = 'Forms'
    Left = 456
    Top = 24
  end
  object QryPesquisa: TFDQuery
    Connection = FDConnection
    Left = 56
    Top = 88
  end
  object DSPesquisa: TDataSource
    DataSet = QryPesquisa
    Left = 56
    Top = 144
  end
  object QryContato: TFDQuery
    Connection = FDConnection
    CachedUpdates = True
    Left = 176
    Top = 88
  end
  object DSContato: TDataSource
    DataSet = QryContato
    Left = 176
    Top = 144
  end
  object QryTelefones: TFDQuery
    Connection = FDConnection
    Left = 296
    Top = 88
  end
  object DSTelefones: TDataSource
    DataSet = QryTelefones
    Left = 296
    Top = 144
  end
  object QryExec: TFDQuery
    Connection = FDConnection
    Left = 416
    Top = 88
  end
end
