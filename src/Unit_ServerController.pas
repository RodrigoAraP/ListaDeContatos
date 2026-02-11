{ ============================================================================
  Unit_ServerController.pas  -  Controlador de Sessoes

  Gerencia o ciclo de vida das sessoes D2Bridge: cria a sessao do
  usuario (TAgendaContatosSession) e o DataModule (TDM) por sessao.

  Quando alguem abre o site no navegador, o D2Bridge dispara OnNewSession.
  Aqui criamos tudo que aquele usuario vai precisar: sessao e banco.
  Quando fecha a aba, OnCloseSession e chamado (limpeza automatica).
  ============================================================================ }
unit Unit_ServerController;

interface

uses
  System.Classes, System.SysUtils,
  D2Bridge.ServerControllerBase, D2Bridge.Types,
  Prism.Session, Prism.Server.HTTP.Commom, Prism.Types, Prism.Interfaces,
  AgendaContatos_Session, { Classe de sessao com dados do usuario }
  Unit_DM;                { DataModule com acesso ao banco }

type
  { Controlador do servidor D2Bridge.
    Herda de TD2BridgeServerControllerBase que fornece toda a infraestrutura
    HTTP, gerenciamento de sessoes e renderizacao de forms para web. }
  TD2BridgeServerController = class(TD2BridgeServerControllerBase)
  private
    { Callback: executado quando um novo usuario acessa o sistema }
    procedure OnNewSession(const Request: TPrismHTTPRequest;
      Response: TPrismHTTPResponse; Session: TPrismSession);

    { Callback: executado quando o usuario fecha a aba }
    procedure OnCloseSession(Session: TPrismSession);
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  { Variavel global que aponta para a instancia unica do controlador }
  D2BridgeServerController: TD2BridgeServerController;

{ Funcao de conveniencia para acessar a sessao do usuario ATUAL.
  Pode ser chamada de qualquer unit para ler/gravar dados da sessao. }
function AppSession: TAgendaContatosSession;

implementation

uses
  D2Bridge.Instance; { Para acessar D2BridgeInstance (contexto da sessao) }

{ Inclui o DFM (design visual) do controlador }
{$R *.dfm}

{ Retorna a sessao do usuario atual fazendo cast do campo generico Data }
function AppSession: TAgendaContatosSession;
begin
  Result := TAgendaContatosSession(D2BridgeInstance.PrismSession.Data);
end;

{ Construtor: registra os callbacks de sessao no engine do D2Bridge }
constructor TD2BridgeServerController.Create(AOwner: TComponent);
begin
  inherited;
  Prism.OnNewSession := OnNewSession;     { Quando usuario entra }
  Prism.OnCloseSession := OnCloseSession; { Quando usuario sai }
end;

{ Executado para CADA novo usuario que acessa o sistema.
  Cria a sessao personalizada e uma instancia do DataModule (banco). }
procedure TD2BridgeServerController.OnNewSession(
  const Request: TPrismHTTPRequest;
  Response: TPrismHTTPResponse; Session: TPrismSession);
begin
  { Cria a classe de sessao e armazena no campo Data da sessao Prism.
    Assim podemos acessar ContatoSelecionadoID de qualquer form. }
  D2BridgeInstance.PrismSession.Data := TAgendaContatosSession.Create(Session);

  { Cria uma instancia do TDM (DataModule) exclusiva para esta sessao.
    Isso garante que cada usuario tem sua propria conexao com o banco,
    evitando conflitos em operacoes simultaneas. }
  D2BridgeInstance.CreateInstance(TDM);
end;

{ Executado quando o usuario fecha a aba.
  O D2Bridge ja destroi automaticamente a sessao e o DataModule. }
procedure TD2BridgeServerController.OnCloseSession(Session: TPrismSession);
begin
  { Limpeza automatica pelo D2Bridge - nao precisa de codigo manual }
end;

end.
