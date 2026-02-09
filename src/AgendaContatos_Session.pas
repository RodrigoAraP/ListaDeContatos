{ ============================================================================
  AgendaContatos_Session.pas  -  Sessao do Usuario

  Armazena dados temporarios por sessao (aba do navegador).
  Cada usuario tem sua propria instancia isolada.
  ============================================================================ }
unit AgendaContatos_Session;

interface

uses
  System.SysUtils, System.Classes,
  Prism.SessionBase;

type
  TAgendaContatosSession = class(TPrismSessionBase)
  public
    ContatoSelecionadoID: Integer;
    ContatoSelecionadoNome: string;
  end;

implementation

end.
