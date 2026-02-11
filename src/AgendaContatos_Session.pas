{ ============================================================================
  AgendaContatos_Session.pas  -  Sessao do Usuario

  Armazena dados temporarios por sessao (aba do navegador).
  Cada usuario tem sua propria instancia isolada.

  No D2Bridge, cada aba do navegador e uma sessao independente.
  Esta classe herda de TPrismSessionBase e serve como "memoria"
  entre navegacoes de tela (ex: passar ID do contato para editar).
  ============================================================================ }
unit AgendaContatos_Session;

interface

uses
  System.SysUtils, System.Classes,
  Prism.SessionBase; { Classe base do D2Bridge para dados de sessao }

type
  { Classe que representa os dados de uma sessao de usuario.
    E criada em OnNewSession e destruida automaticamente ao fechar a aba. }
  TAgendaContatosSession = class(TPrismSessionBase)
  public
    { ID do contato selecionado para edicao (0 = novo contato) }
    ContatoSelecionadoID: Integer;

    { Nome do contato (usado para exibir no titulo da tela de edicao) }
    ContatoSelecionadoNome: string;
  end;

implementation

end.
