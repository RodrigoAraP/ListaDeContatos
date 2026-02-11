{ ============================================================================
  AgendaContatos  -  Agenda Telefonica Web

  Aplicacao web de agenda telefonica com CRUD completo.
  Desenvolvido com Delphi 12 + D2Bridge Framework + MySQL 8.

  Este e o arquivo principal do projeto Delphi (.dpr = Delphi Project).
  Equivale ao "main()" em outras linguagens.
  Ele registra todas as units utilizadas e inicializa a aplicacao.
  ============================================================================ }
program AgendaContatos;

uses
  { Unit padrao do Delphi para gerenciamento de formularios VCL }
  Vcl.Forms,

  { Form VCL que controla o servidor (tem botao Iniciar/Parar) }
  Unit_D2BridgeServer in 'src\Unit_D2BridgeServer.pas' {FormServer},

  { Controlador de sessoes - cria sessao e DataModule por usuario }
  Unit_ServerController in 'src\Unit_ServerController.pas' {D2BridgeServerController: TDataModule},

  { Classe de sessao - armazena dados temporarios do usuario }
  AgendaContatos_Session in 'src\AgendaContatos_Session.pas',

  { DataModule - toda a logica de acesso ao banco MySQL }
  Unit_DM in 'src\Unit_DM.pas' {DM: TDataModule},

  { Tela Home - listagem e pesquisa de contatos }
  Unit_FormHome in 'src\Unit_FormHome.pas' {FormHome},

  { Tela Contato - criar/editar contato e gerenciar telefones }
  Unit_FormContato in 'src\Unit_FormContato.pas' {FormContato},

  { Utilitario de log - grava operacoes em arquivo texto }
  Unit_Log in 'src\Unit_Log.pas';

{ Inclui o arquivo de recursos (.res) que contem icone e metadados }
{$R *.res}

begin
  { Inicializa o subsistema de aplicacao do Delphi }
  Application.Initialize;

  { Permite que o form principal apareca na barra de tarefas do Windows }
  Application.MainFormOnTaskbar := True;

  { Cria o formulario do servidor - e o unico form criado automaticamente.
    Os forms web (Home, Contato) sao criados pelo D2Bridge por sessao. }
  Application.CreateForm(TFormServer, FormServer);

  { Inicia o loop de mensagens do Windows (mantém a aplicacao rodando) }
  Application.Run;
end.
