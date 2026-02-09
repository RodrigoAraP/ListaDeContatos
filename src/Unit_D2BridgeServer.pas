{ ============================================================================
  Unit_D2BridgeServer.pas  -  Form de Controle do Servidor

  Interface VCL para iniciar/parar o servidor D2Bridge HTTP.
  Exibe log de eventos e status da conexao.
  ============================================================================ }
unit Unit_D2BridgeServer;

interface

uses
  System.SysUtils, System.Classes,
  Vcl.Controls, Vcl.Forms,
  Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TFormServer = class(TForm)
    PanelTop: TPanel;
    LabelTitle: TLabel;
    LabelPort: TLabel;
    EditPort: TEdit;
    BtnStart: TButton;
    BtnStop: TButton;
    LabelStatus: TLabel;
    MemoLog: TMemo;

    procedure FormCreate(Sender: TObject);
    procedure BtnStartClick(Sender: TObject);
    procedure BtnStopClick(Sender: TObject);
  private
    procedure Log(const AMsg: string);
  end;

var
  FormServer: TFormServer;

implementation

{$R *.dfm}

uses
  Unit_ServerController, Unit_FormHome;

procedure TFormServer.FormCreate(Sender: TObject);
begin
  D2BridgeServerController := TD2BridgeServerController.Create(Application);
  Log('Servidor criado. Clique em Iniciar.');
end;

procedure TFormServer.BtnStartClick(Sender: TObject);
begin
  try
    D2BridgeServerController.PrimaryFormClass := TFormHome;

    { Habilitar jQuery e demais bibliotecas }
    D2BridgeServerController.Prism.Options.IncludeJQuery := True;

    D2BridgeServerController.Port := StrToIntDef(EditPort.Text, 8080);
    D2BridgeServerController.StartServer;

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

procedure TFormServer.Log(const AMsg: string);
begin
  MemoLog.Lines.Add(Format('[%s] %s',
    [FormatDateTime('hh:nn:ss', Now), AMsg]));
end;

end.
