{ ============================================================================
  AgendaContatos  -  Agenda Telefonica Web

  Aplicacao web de agenda telefonica com CRUD completo.
  Desenvolvido com Delphi 12 + D2Bridge Framework + MySQL 8.
  ============================================================================ }
program AgendaContatos;

uses
  Vcl.Forms,
  Unit_D2BridgeServer in 'src\Unit_D2BridgeServer.pas' {FormServer},
  Unit_ServerController in 'src\Unit_ServerController.pas' {D2BridgeServerController: TDataModule},
  AgendaContatos_Session in 'src\AgendaContatos_Session.pas',
  Unit_DM in 'src\Unit_DM.pas' {DM: TDataModule},
  Unit_FormHome in 'src\Unit_FormHome.pas' {FormHome},
  Unit_FormContato in 'src\Unit_FormContato.pas' {FormContato},
  Unit_Log in 'src\Unit_Log.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormServer, FormServer);
  Application.Run;
end.
