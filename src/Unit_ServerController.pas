{ ============================================================================
  Unit_ServerController.pas  -  Controlador de Sessoes

  Gerencia o ciclo de vida das sessoes D2Bridge: cria a sessao do
  usuario (TAgendaContatosSession) e o DataModule (TDM) por sessao.
  ============================================================================ }
unit Unit_ServerController;

interface

uses
  System.Classes, System.SysUtils,
  D2Bridge.ServerControllerBase, D2Bridge.Types,
  Prism.Session, Prism.Server.HTTP.Commom, Prism.Types, Prism.Interfaces,
  AgendaContatos_Session,
  Unit_DM;

type
  TD2BridgeServerController = class(TD2BridgeServerControllerBase)
  private
    procedure OnNewSession(const Request: TPrismHTTPRequest;
      Response: TPrismHTTPResponse; Session: TPrismSession);
    procedure OnCloseSession(Session: TPrismSession);
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  D2BridgeServerController: TD2BridgeServerController;

{ Funcao global para acessar a sessao atual }
function AppSession: TAgendaContatosSession;

implementation

uses
  D2Bridge.Instance;

{$R *.dfm}

function AppSession: TAgendaContatosSession;
begin
  Result := TAgendaContatosSession(D2BridgeInstance.PrismSession.Data);
end;

constructor TD2BridgeServerController.Create(AOwner: TComponent);
begin
  inherited;
  Prism.OnNewSession := OnNewSession;
  Prism.OnCloseSession := OnCloseSession;
end;

procedure TD2BridgeServerController.OnNewSession(
  const Request: TPrismHTTPRequest;
  Response: TPrismHTTPResponse; Session: TPrismSession);
begin
  { Criar dados de sessao }
  D2BridgeInstance.PrismSession.Data := TAgendaContatosSession.Create(Session);

  { Criar instancia do DataModule para esta sessao }
  D2BridgeInstance.CreateInstance(TDM);
end;

procedure TD2BridgeServerController.OnCloseSession(Session: TPrismSession);
begin
  { Limpeza automatica pelo D2Bridge }
end;

end.
