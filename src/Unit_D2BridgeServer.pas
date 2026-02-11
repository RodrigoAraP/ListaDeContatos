{ ============================================================================
  Unit_D2BridgeServer.pas  -  Form de Controle do Servidor

  Interface VCL para iniciar/parar o servidor D2Bridge HTTP.
  Exibe log de eventos e status da conexao.

  Este e o UNICO formulario VCL nativo da aplicacao. Ele nao aparece
  no navegador - serve apenas como painel de controle para o desenvolvedor
  iniciar/parar o servidor HTTP que atende as requisicoes web.
  ============================================================================ }
unit Unit_D2BridgeServer;

interface

uses
  System.SysUtils, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.Graphics,
  Vcl.StdCtrls, Vcl.ExtCtrls;

type
  { Form VCL com painel de controle do servidor D2Bridge.
    Componentes: campo de porta, botoes Start/Stop, label de status e memo de log. }
  TFormServer = class(TForm)
    PanelTop: TPanel;       { Painel superior com titulo e controles }
    LabelTitle: TLabel;     { Titulo "Agenda de Contatos - Servidor" }
    LabelPort: TLabel;      { Label "Porta:" }
    EditPort: TEdit;        { Campo para digitar a porta (padrao: 8080) }
    BtnStart: TButton;      { Botao para iniciar o servidor HTTP }
    BtnStop: TButton;       { Botao para parar o servidor HTTP }
    LabelStatus: TLabel;    { Exibe "Rodando" (verde) ou "Parado" (vermelho) }
    MemoLog: TMemo;         { Area de texto que exibe o log de eventos do servidor }

    procedure FormCreate(Sender: TObject);
    procedure BtnStartClick(Sender: TObject);
    procedure BtnStopClick(Sender: TObject);
  private
    { Adiciona uma linha timestamped ao MemoLog }
    procedure Log(const AMsg: string);
  end;

var
  FormServer: TFormServer;

implementation

{$R *.dfm}

uses
  Unit_ServerController, { Para acessar D2BridgeServerController }
  Unit_FormHome;         { Para definir TFormHome como tela inicial }

{ Executado ao criar o formulario.
  Cria a instancia do controlador D2Bridge e registra no log. }
procedure TFormServer.FormCreate(Sender: TObject);
begin
  D2BridgeServerController := TD2BridgeServerController.Create(Application);
  Log('Servidor criado. Clique em Iniciar.');
end;

{ Ao clicar em "Iniciar":
  1. Define qual form web sera a tela inicial (TFormHome)
  2. Habilita jQuery (necessario para componentes D2Bridge)
  3. Configura a porta e inicia o servidor HTTP
  4. Atualiza a interface (botoes, status, log) }
procedure TFormServer.BtnStartClick(Sender: TObject);
begin
  try
    { Define a primeira tela que o usuario ve ao acessar http://localhost:porta }
    D2BridgeServerController.PrimaryFormClass := TFormHome;

    { jQuery e necessario para os componentes interativos do D2Bridge }
    D2BridgeServerController.Prism.Options.IncludeJQuery := True;

    { Converte o texto da porta para inteiro (se invalido, usa 8080) }
    D2BridgeServerController.Port := StrToIntDef(EditPort.Text, 8080);

    { Inicia o servidor HTTP - a partir daqui aceita conexoes }
    D2BridgeServerController.StartServer;

    { Atualiza a interface para refletir o estado "rodando" }
    BtnStart.Enabled := False;
    BtnStop.Enabled := True;
    LabelStatus.Caption := Format('Rodando em http://localhost:%s', [EditPort.Text]);
    LabelStatus.Font.Color := clGreen;
    Log('Servidor iniciado na porta ' + EditPort.Text);
    Log('Acesse: http://localhost:' + EditPort.Text);
  except
    on E: Exception do
    begin
      LabelStatus.Caption := 'Erro ao iniciar';
      LabelStatus.Font.Color := clRed;
      Log('ERRO: ' + E.Message);
    end;
  end;
end;

{ Ao clicar em "Parar": encerra o servidor HTTP e atualiza a interface }
procedure TFormServer.BtnStopClick(Sender: TObject);
begin
  try
    D2BridgeServerController.StopServer;
    BtnStart.Enabled := True;
    BtnStop.Enabled := False;
    LabelStatus.Caption := 'Parado';
    LabelStatus.Font.Color := clRed;
    Log('Servidor parado.');
  except
    on E: Exception do
      Log('ERRO ao parar: ' + E.Message);
  end;
end;

{ Adiciona uma linha formatada com hora ao memo de log }
procedure TFormServer.Log(const AMsg: string);
begin
  MemoLog.Lines.Add(Format('[%s] %s',
    [FormatDateTime('hh:nn:ss', Now), AMsg]));
end;

end.
