{ ============================================================================
  Unit_FormContato.pas  -  Tela de Contato (Criar / Editar)

  Formulario para cadastro e edicao de contatos com gerenciamento
  de multiplos telefones. Cada telefone e salvo imediatamente no banco.
  ============================================================================ }
unit Unit_FormContato;

interface

uses
  System.SysUtils, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.DBCtrls, Vcl.Grids, Vcl.DBGrids,
  Data.DB,
  D2Bridge.Forms;

type
  TFormContato = class(TD2BridgeForm)
    { Campos do contato }
    DBEditNome: TDBEdit;
    DBEditIdade: TDBEdit;
    LabelNome: TLabel;
    LabelIdade: TLabel;

    { Telefones }
    LabelTelefone: TLabel;
    EditTelefone: TEdit;
    BtnAdicionarTelefone: TButton;
    BtnRemoverTelefone: TButton;
    DBGridTelefones: TDBGrid;

    { Acoes }
    BtnSalvar: TButton;
    BtnCancelar: TButton;
    BtnExcluirContato: TButton;

    { Eventos do DFM }
    procedure BtnSalvarClick(Sender: TObject);
    procedure BtnCancelarClick(Sender: TObject);
    procedure BtnAdicionarTelefoneClick(Sender: TObject);
    procedure BtnRemoverTelefoneClick(Sender: TObject);
    procedure BtnExcluirContatoClick(Sender: TObject);
  private
    FContatoID: Integer;
    LabelTitulo: TLabel;
    procedure ConectarDataSources;
    procedure VoltarParaHome;
    function  GarantirContatoSalvo: Boolean;
    procedure RefreshTelefones;
    procedure DoPageLoaded(Sender: TObject);
    procedure ExcluirTelefoneDaLinha;
  protected
    procedure ExportD2Bridge; override;
    procedure InitControlsD2Bridge(const PrismControl: TPrismControl); override;
  end;

function FormContato: TFormContato;

implementation

{$R *.dfm}

uses
  Unit_DM, Unit_Log, Unit_ServerController, Unit_FormHome;

function FormContato: TFormContato;
begin
  Result := TFormContato(TFormContato.GetInstance);
end;

{ ========================================================================== }
{  EXPORT D2BRIDGE - Layout compacto                                          }
{ ========================================================================== }
procedure TFormContato.ExportD2Bridge;
begin
  inherited;

  FContatoID := AppSession.ContatoSelecionadoID;

  if FContatoID > 0 then
    Title := 'Editar Contato'
  else
    Title := 'Novo Contato';

  Self.OnPageLoaded := DoPageLoaded;

  { Label do titulo (criado dinamicamente para H2) }
  LabelTitulo := TLabel.Create(Self);
  if FContatoID > 0 then
    LabelTitulo.Caption := 'Editar: ' + AppSession.ContatoSelecionadoNome
  else
    LabelTitulo.Caption := 'Novo Contato';

  { Conectar DataSources ANTES de adicionar componentes ao D2Bridge }
  DM.EnsureConnected;
  ConectarDataSources;

  if FContatoID > 0 then
    DM.AbrirTelefones(FContatoID)
  else
    DM.AbrirTelefones(0);

  with D2Bridge.Items.Add do
  begin
   { Container com largura maxima de 1600px centralizado }
   with HTMLDIV('', '', '', 'max-width:1600px; margin:0 auto; width:100%').Items.Add do
   begin
    { H2 - Titulo da pagina }
    with HTMLDIV('mb-3', '', '', '', 'h2').Items.Add do
      VCLObj(LabelTitulo);

    { Linha 1: Nome + Idade lado a lado }
    with Row.Items.Add do
    begin
      with FormGroup('Nome *', CSSClass.Col.colsize6) do
        AddVCLObj(DBEditNome);
      with FormGroup('Idade', CSSClass.Col.colsize2) do
        AddVCLObj(DBEditIdade);
    end;

    { Linha 2: Campo telefone + botao adicionar }
    with Row.Items.Add do
    begin
      with FormGroup('Telefone', CSSClass.Col.colsize4) do
        AddVCLObj(EditTelefone);
      ColAuto.Add.VCLObj(BtnAdicionarTelefone, CSSClass.Button.add);
    end;

    { Linha 3: Grid de telefones (compacto, col-6) com gap acima }
    with HTMLDIV('mt-3').Items.Add do
      VCLObj(DBGridTelefones, CSSClass.Col.colsize6);

    { Linha 4: Botoes - Salvar e Voltar a esquerda, Excluir a direita }
    with HTMLDIV(CSSClass.DivHtml.Align_Between).Items.Add do
    begin
      with HTMLDIV(CSSClass.DivHtml.Align_Left).Items.Add do
      begin
        ColAuto.Add.VCLObj(BtnSalvar, CSSClass.Button.save);
        ColAuto.Add.VCLObj(BtnCancelar, CSSClass.Button.close);
      end;
      { Botao excluir contato - so aparece em edicao }
      if FContatoID > 0 then
        ColAuto.Add.VCLObj(BtnExcluirContato, CSSClass.Button.delete);
    end;
   end;
  end;
end;

{ ========================================================================== }
{  INIT CONTROLS - Lixeira por linha no grid de telefones                     }
{ ========================================================================== }
procedure TFormContato.InitControlsD2Bridge(const PrismControl: TPrismControl);
begin
  inherited;

  if PrismControl.VCLComponent = DBGridTelefones then
  begin
    with PrismControl.AsDBGrid do
    begin
      with Columns.Add do
      begin
        Title := '';
        Width := 30;

        with Buttons.Add do
        begin
          ButtonModel := TButtonModel.Delete;
          ClickProc := procedure
          begin
            ExcluirTelefoneDaLinha;
          end;
        end;
      end;
    end;
  end;
end;

{ ========================================================================== }
{  EVENTOS                                                                    }
{ ========================================================================== }

procedure TFormContato.DoPageLoaded(Sender: TObject);
begin
  FContatoID := AppSession.ContatoSelecionadoID;

  if FContatoID > 0 then
  begin
    DM.AbrirContato(FContatoID);
    DM.AbrirTelefones(FContatoID);
    if not DM.QryContato.IsEmpty then
      DM.QryContato.Edit;
  end
  else
  begin
    DM.InserirContato;
    DM.AbrirTelefones(0);
  end;

  EditTelefone.Text := '';
end;

{ ========================================================================== }
{  DATASOURCES                                                                }
{ ========================================================================== }

procedure TFormContato.ConectarDataSources;
begin
  DBEditNome.DataSource      := DM.DSContato;
  DBEditIdade.DataSource     := DM.DSContato;
  DBGridTelefones.DataSource := DM.DSTelefones;
end;

{ ========================================================================== }
{  TELEFONES                                                                  }
{ ========================================================================== }

function TFormContato.GarantirContatoSalvo: Boolean;
begin
  Result := False;

  if FContatoID > 0 then
  begin
    if DM.QryContato.Active and (DM.QryContato.State in [dsEdit, dsInsert]) then
      DM.SalvarContato;
    Result := True;
    Exit;
  end;

  if Trim(DBEditNome.Text) = '' then
  begin
    ShowMessage('Preencha o Nome antes de adicionar telefones.', True, 4000, mtWarning);
    Exit;
  end;

  FContatoID := DM.SalvarContatoRetornarID;
  if FContatoID > 0 then
  begin
    GravarLogCriacao(FContatoID, Trim(DBEditNome.Text),
      StrToIntDef(Trim(DBEditIdade.Text), 0));
    DM.AbrirContato(FContatoID);
    if not DM.QryContato.IsEmpty then
      DM.QryContato.Edit;
    AppSession.ContatoSelecionadoID := FContatoID;
    Result := True;
  end
  else
    ShowMessage('Erro ao salvar o contato.', True, 5000, mtError);
end;

procedure TFormContato.RefreshTelefones;
begin
  if FContatoID > 0 then
    DM.AbrirTelefones(FContatoID);
end;

procedure TFormContato.ExcluirTelefoneDaLinha;
var
  IDTelefone: Integer;
  Numero: string;
begin
  if not DM.QryTelefones.Active or DM.QryTelefones.IsEmpty then Exit;

  IDTelefone := DM.QryTelefones.FieldByName('ID').AsInteger;
  Numero := DM.QryTelefones.FieldByName('NUMERO').AsString;

  DM.ExcluirTelefonePorID(IDTelefone);
  GravarLog(tlTelefoneRemove,
    Format('Telefone removido - ContatoID: %d | Numero: %s', [FContatoID, Numero]));
  RefreshTelefones;

  ShowMessage('Telefone removido.', True);
end;

procedure TFormContato.BtnAdicionarTelefoneClick(Sender: TObject);
var
  Numero: string;
begin
  Numero := Trim(EditTelefone.Text);

  if Numero = '' then
  begin
    ShowMessage('Digite o numero do telefone.', True, 3000, mtWarning);
    Exit;
  end;

  if not GarantirContatoSalvo then
    Exit;

  DM.InserirTelefoneSQL(FContatoID, Numero);
  GravarLog(tlTelefoneAdd,
    Format('Telefone adicionado - ContatoID: %d | Numero: %s', [FContatoID, Numero]));

  RefreshTelefones;
  EditTelefone.Text := '';

  ShowMessage('Telefone adicionado.', True);
end;

procedure TFormContato.BtnRemoverTelefoneClick(Sender: TObject);
begin
  ExcluirTelefoneDaLinha;
end;

{ ========================================================================== }
{  SALVAR / CANCELAR / EXCLUIR                                                }
{ ========================================================================== }

procedure TFormContato.BtnSalvarClick(Sender: TObject);
begin
  if Trim(DBEditNome.Text) = '' then
  begin
    ShowMessage('O campo Nome e obrigatorio.', True, 4000, mtWarning);
    Exit;
  end;

  if FContatoID = 0 then
  begin
    FContatoID := DM.SalvarContatoRetornarID;
    if FContatoID = 0 then
    begin
      ShowMessage('Erro ao salvar o contato.', True, 5000, mtError);
      Exit;
    end;
    GravarLogCriacao(FContatoID, Trim(DBEditNome.Text),
      StrToIntDef(Trim(DBEditIdade.Text), 0));
  end
  else
  begin
    if not DM.SalvarContato then
    begin
      ShowMessage('Erro ao salvar o contato.', True, 5000, mtError);
      Exit;
    end;
    GravarLogEdicao(FContatoID, Trim(DBEditNome.Text),
      StrToIntDef(Trim(DBEditIdade.Text), 0));
  end;

  ShowMessage('Contato salvo com sucesso!', True);
  VoltarParaHome;
end;

procedure TFormContato.BtnCancelarClick(Sender: TObject);
begin
  if DM.QryContato.Active and (DM.QryContato.State in [dsEdit, dsInsert]) then
    DM.QryContato.Cancel;

  VoltarParaHome;
end;

procedure TFormContato.BtnExcluirContatoClick(Sender: TObject);
var
  Nome, Telefones: string;
  Idade: Integer;
begin
  if FContatoID = 0 then Exit;

  { Coleta nome para exibir no dialogo }
  Nome := '';
  if DM.QryContato.Active and not DM.QryContato.IsEmpty then
    Nome := DM.QryContato.FieldByName('NOME').AsString;

  { Dialogo de confirmacao }
  if MessageDlg('Deseja realmente excluir o contato "' + Nome + '"?',
    TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0) <> mrYes then
    Exit;

  { Coleta dados completos para log }
  Idade := 0;
  Telefones := '';
  if DM.QryContato.Active and not DM.QryContato.IsEmpty then
    Idade := DM.QryContato.FieldByName('IDADE').AsInteger;

  if DM.QryTelefones.Active then
  begin
    DM.QryTelefones.First;
    while not DM.QryTelefones.Eof do
    begin
      if Telefones <> '' then
        Telefones := Telefones + ', ';
      Telefones := Telefones + DM.QryTelefones.FieldByName('NUMERO').AsString;
      DM.QryTelefones.Next;
    end;
  end;

  GravarLogExclusao(FContatoID, Nome, Idade, Telefones);
  DM.ExcluirContato(FContatoID);

  ShowMessage('Contato "' + Nome + '" excluido com sucesso.', True);
  VoltarParaHome;
end;

{ ========================================================================== }
{  NAVEGACAO                                                                  }
{ ========================================================================== }

procedure TFormContato.VoltarParaHome;
begin
  AppSession.ContatoSelecionadoID := 0;
  FContatoID := 0;
  DM.PesquisarContatos('', '');
  FormHome.Show;
end;

end.
