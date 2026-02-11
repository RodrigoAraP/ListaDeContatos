{ ============================================================================
  Unit_FormContato.pas  -  Tela de Contato (Criar / Editar)

  Formulario para cadastro e edicao de contatos com gerenciamento
  de multiplos telefones. Cada telefone e salvo imediatamente no banco.

  Conceitos-chave:
  - Modo dual: forma serve tanto para CRIAR (ID=0) quanto EDITAR (ID>0)
  - GarantirContatoSalvo: auto-salva contato antes de adicionar telefone
  - Telefones sao INSERT/DELETE imediato (sem CachedUpdates)
  - ON DELETE CASCADE: ao excluir contato, MySQL remove telefones sozinho
  - VoltarParaHome: limpa sessao, recarrega pesquisa e muda de tela
  ============================================================================ }
unit Unit_FormContato;

interface

uses
  System.SysUtils, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.DBCtrls, Vcl.Grids, Vcl.DBGrids,
  Data.DB,
  D2Bridge.Forms; { TD2BridgeForm e tipos auxiliares do framework }

type
  TFormContato = class(TD2BridgeForm)

    { === Campos do contato (data-aware = conectados ao DataSource) === }
    DBEditNome: TDBEdit;     { Campo texto ligado ao campo NOME da query }
    DBEditIdade: TDBEdit;    { Campo texto ligado ao campo IDADE da query }
    LabelNome: TLabel;       { Rotulo "Nome *" }
    LabelIdade: TLabel;      { Rotulo "Idade" }

    { === Secao de Telefones === }
    LabelTelefone: TLabel;         { Rotulo "Telefone" }
    EditTelefone: TEdit;           { Campo texto para digitar novo numero }
    BtnAdicionarTelefone: TButton; { Botao "+" adicionar telefone }
    BtnRemoverTelefone: TButton;   { Botao remover (nao usado - grid tem lixeira) }
    DBGridTelefones: TDBGrid;      { Grid com lista de telefones do contato }

    { === Botoes de acao === }
    BtnSalvar: TButton;          { Salvar contato e voltar }
    BtnCancelar: TButton;        { Cancelar alteracoes e voltar }
    BtnExcluirContato: TButton;  { Excluir contato (so em modo edicao) }

    { Eventos definidos no DFM (OnClick) }
    procedure BtnSalvarClick(Sender: TObject);
    procedure BtnCancelarClick(Sender: TObject);
    procedure BtnAdicionarTelefoneClick(Sender: TObject);
    procedure BtnRemoverTelefoneClick(Sender: TObject);
    procedure BtnExcluirContatoClick(Sender: TObject);
  private
    { ID do contato sendo editado. 0 = novo contato, >0 = editando existente }
    FContatoID: Integer;

    { Label criado em runtime para exibir como tag <h2> }
    LabelTitulo: TLabel;

    { Liga os DBEdits/DBGrid ao DataModule }
    procedure ConectarDataSources;

    { Limpa sessao e navega de volta ao FormHome }
    procedure VoltarParaHome;

    { Auto-salva contato novo (se necessario) antes de adicionar telefone.
      Retorna True se o contato tem ID valido (ja salvo no banco). }
    function  GarantirContatoSalvo: Boolean;

    { Recarrega a query de telefones apos INSERT ou DELETE }
    procedure RefreshTelefones;

    { Callback executado a cada navegacao para esta pagina }
    procedure DoPageLoaded(Sender: TObject);

    { Exclui o telefone da linha atual do grid (icone lixeira) }
    procedure ExcluirTelefoneDaLinha;
  protected
    { Monta o layout HTML/Bootstrap da tela uma vez }
    procedure ExportD2Bridge; override;

    { Adiciona botao lixeira por linha no grid de telefones }
    procedure InitControlsD2Bridge(const PrismControl: TPrismControl); override;
  end;

{ Funcao global - retorna instancia do FormContato da sessao atual }
function FormContato: TFormContato;

implementation

{$R *.dfm}

uses
  Unit_DM,               { Acesso a dados (MySQL) }
  Unit_Log,              { Gravacao de logs de alteracao }
  Unit_ServerController, { AppSession - dados da sessao }
  Unit_FormHome;         { FormHome - tela de listagem }

{ Obtem a instancia UNICA deste form na sessao D2Bridge atual }
function FormContato: TFormContato;
begin
  Result := TFormContato(TFormContato.GetInstance);
end;

{ ========================================================================== }
{  EXPORT D2BRIDGE - Layout da pagina de Contato                              }
{                                                                             }
{  Define a estrutura visual: campos Nome/Idade, campo Telefone com botao,    }
{  grid de telefones, e botoes Salvar/Voltar/Excluir.                         }
{  A tela adapta o titulo e botoes conforme o MODO (novo vs edicao).          }
{ ========================================================================== }
procedure TFormContato.ExportD2Bridge;
begin
  inherited;

  { Le o ID da sessao para saber se e criacao (0) ou edicao (>0) }
  FContatoID := AppSession.ContatoSelecionadoID;

  { Define o titulo da aba do navegador conforme o modo }
  if FContatoID > 0 then
    Title := 'Editar Contato'
  else
    Title := 'Novo Contato';

  { Registra callback para recarregar dados a cada navegacao }
  Self.OnPageLoaded := DoPageLoaded;

  { Cria label H2 dinamicamente com texto contextual }
  LabelTitulo := TLabel.Create(Self);
  if FContatoID > 0 then
    LabelTitulo.Caption := 'Editar: ' + AppSession.ContatoSelecionadoNome
  else
    LabelTitulo.Caption := 'Novo Contato';

  { Conectar DataSources ANTES de adicionar componentes ao layout D2Bridge.
    Sem isso, os DBEdits e DBGrid nao sabem quais campos exibir. }
  DM.EnsureConnected;
  ConectarDataSources;

  { Carrega telefones do contato (ou query vazia se novo) }
  if FContatoID > 0 then
    DM.AbrirTelefones(FContatoID)
  else
    DM.AbrirTelefones(0); { Passa 0 = retorna vazio }

  { === Inicio da arvore de layout === }
  with D2Bridge.Items.Add do
  begin
   { Container com largura maxima de 1600px, centralizado }
   with HTMLDIV('', '', '', 'max-width:1600px; margin:0 auto; width:100%').Items.Add do
   begin
    { Titulo H2 com margin-bottom }
    with HTMLDIV('mb-3', '', '', '', 'h2').Items.Add do
      VCLObj(LabelTitulo);

    { Linha 1: Nome (col-6) + Idade (col-2) lado a lado.
      FormGroup renderiza como <div class="form-group col-X"> com label. }
    with Row.Items.Add do
    begin
      with FormGroup('Nome *', CSSClass.Col.colsize6) do
        AddVCLObj(DBEditNome);   { Renderiza como <input> ligado ao campo NOME }
      with FormGroup('Idade', CSSClass.Col.colsize2) do
        AddVCLObj(DBEditIdade);  { Renderiza como <input> ligado ao campo IDADE }
    end;

    { Linha 2: Campo telefone + botao "+" para adicionar }
    with Row.Items.Add do
    begin
      with FormGroup('Telefone', CSSClass.Col.colsize4) do
        AddVCLObj(EditTelefone); { TEdit simples (nao data-aware) }
      { CSSClass.Button.add renderiza icone "+" no botao }
      ColAuto.Add.VCLObj(BtnAdicionarTelefone, CSSClass.Button.add);
    end;

    { Linha 3: Grid de telefones compacto (col-6) com espacamento acima.
      colsize6 = ocupa metade da largura (6 de 12 colunas Bootstrap). }
    with HTMLDIV('mt-3').Items.Add do
      VCLObj(DBGridTelefones, CSSClass.Col.colsize6);

    { Linha 4: Botoes de acao distribuidos com espaco entre eles.
      Align_Between = justify-content: space-between (CSS Flexbox). }
    with HTMLDIV(CSSClass.DivHtml.Align_Between).Items.Add do
    begin
      { Lado esquerdo: Salvar + Voltar }
      with HTMLDIV(CSSClass.DivHtml.Align_Left).Items.Add do
      begin
        ColAuto.Add.VCLObj(BtnSalvar, CSSClass.Button.save);    { Icone de disquete }
        ColAuto.Add.VCLObj(BtnCancelar, CSSClass.Button.close);  { Icone de X }
      end;

      { Lado direito: botao Excluir (apenas no modo edicao).
        Em modo criacao (ID=0) nao faz sentido excluir. }
      if FContatoID > 0 then
        ColAuto.Add.VCLObj(BtnExcluirContato, CSSClass.Button.delete);
    end;
   end;
  end;
end;

{ ========================================================================== }
{  INIT CONTROLS - Lixeira por linha no grid de telefones                     }
{                                                                             }
{  Adiciona uma coluna extra no DBGridTelefones com um botao de excluir       }
{  (icone lixeira) em cada linha. Ao clicar, chama ExcluirTelefoneDaLinha.   }
{ ========================================================================== }
procedure TFormContato.InitControlsD2Bridge(const PrismControl: TPrismControl);
begin
  inherited;

  { Verifica se o componente sendo configurado e o grid de telefones }
  if PrismControl.VCLComponent = DBGridTelefones then
  begin
    with PrismControl.AsDBGrid do
    begin
      { Adiciona coluna extra sem titulo, com 30px de largura }
      with Columns.Add do
      begin
        Title := '';
        Width := 30;

        { Botao lixeira em cada linha do grid }
        with Buttons.Add do
        begin
          ButtonModel := TButtonModel.Delete; { Icone de lixeira }
          ClickProc := procedure
          begin
            ExcluirTelefoneDaLinha; { Remove o telefone da linha clicada }
          end;
        end;
      end;
    end;
  end;
end;

{ ========================================================================== }
{  EVENTOS DE PAGINA                                                          }
{                                                                             }
{  DoPageLoaded: executado TODA vez que o usuario navega para esta pagina.     }
{  Decide se abre um contato existente ou prepara para insercao.             }
{ ========================================================================== }

procedure TFormContato.DoPageLoaded(Sender: TObject);
begin
  FContatoID := AppSession.ContatoSelecionadoID;

  if FContatoID > 0 then
  begin
    { === MODO EDICAO: abre o contato e seus telefones === }
    DM.AbrirContato(FContatoID);
    DM.AbrirTelefones(FContatoID);
    { Coloca a query em modo Edit para o usuario poder alterar }
    if not DM.QryContato.IsEmpty then
      DM.QryContato.Edit;
  end
  else
  begin
    { === MODO CRIACAO: prepara query vazia com Append === }
    DM.InserirContato;  { WHERE 1=0 + Append }
    DM.AbrirTelefones(0); { Grid de telefones vazio }
  end;

  EditTelefone.Text := ''; { Limpa campo de telefone }
end;

{ ========================================================================== }
{  DATASOURCES - Conecta componentes visuais ao DataModule                    }
{ ========================================================================== }

procedure TFormContato.ConectarDataSources;
begin
  { DBEditNome e DBEditIdade conectam ao DSContato -> QryContato }
  DBEditNome.DataSource      := DM.DSContato;
  DBEditIdade.DataSource     := DM.DSContato;
  { DBGridTelefones conecta ao DSTelefones -> QryTelefones }
  DBGridTelefones.DataSource := DM.DSTelefones;
end;

{ ========================================================================== }
{  GERENCIAMENTO DE TELEFONES                                                 }
{                                                                             }
{  Telefones sao salvos IMEDIATAMENTE no banco (sem CachedUpdates).           }
{  Se o contato ainda nao foi salvo, GarantirContatoSalvo faz o INSERT       }
{  automatico do contato antes de inserir o telefone.                         }
{ ========================================================================== }

{ GarantirContatoSalvo: garante que o contato tem um ID no banco.
  - Se ID > 0 (edicao): apenas faz Post se necessario, retorna True.
  - Se ID = 0 (novo): valida nome, salva contato e obtem o novo ID.
  Retorna True se o contato esta salvo e pronto para receber telefones. }

function TFormContato.GarantirContatoSalvo: Boolean;
begin
  Result := False;

  if FContatoID > 0 then
  begin
    { Contato ja existe - apenas garante que mudancas pendentes foram salvas }
    if DM.QryContato.Active and (DM.QryContato.State in [dsEdit, dsInsert]) then
      DM.SalvarContato;
    Result := True;
    Exit;
  end;

  { Contato novo - precisa de nome para salvar }
  if Trim(DBEditNome.Text) = '' then
  begin
    ShowMessage('Preencha o Nome antes de adicionar telefones.', True, 4000, mtWarning);
    Exit;
  end;

  { Salva o contato e obtem o ID gerado pelo AUTO_INCREMENT }
  FContatoID := DM.SalvarContatoRetornarID;
  if FContatoID > 0 then
  begin
    { Grava log de criacao com os dados do contato }
    GravarLogCriacao(FContatoID, Trim(DBEditNome.Text),
      StrToIntDef(Trim(DBEditIdade.Text), 0));

    { Reabre o contato para poder continuar editando }
    DM.AbrirContato(FContatoID);
    if not DM.QryContato.IsEmpty then
      DM.QryContato.Edit;

    { Atualiza sessao com o novo ID }
    AppSession.ContatoSelecionadoID := FContatoID;
    Result := True;
  end
  else
    ShowMessage('Erro ao salvar o contato.', True, 5000, mtError);
end;

{ Recarrega a lista de telefones do contato }
procedure TFormContato.RefreshTelefones;
begin
  if FContatoID > 0 then
    DM.AbrirTelefones(FContatoID);
end;

{ Exclui o telefone que esta na linha ATUAL do grid.
  Chamado pelo botao lixeira (ClickProc no InitControlsD2Bridge). }
procedure TFormContato.ExcluirTelefoneDaLinha;
var
  IDTelefone: Integer;
  Numero: string;
begin
  if not DM.QryTelefones.Active or DM.QryTelefones.IsEmpty then Exit;

  { Le os dados da linha atual ANTES de excluir }
  IDTelefone := DM.QryTelefones.FieldByName('ID').AsInteger;
  Numero := DM.QryTelefones.FieldByName('NUMERO').AsString;

  { DELETE direto no MySQL }
  DM.ExcluirTelefonePorID(IDTelefone);

  { Grava log da remocao do telefone }
  GravarLog(tlTelefoneRemove,
    Format('Telefone removido - ContatoID: %d | Numero: %s', [FContatoID, Numero]));

  { Atualiza o grid }
  RefreshTelefones;

  ShowMessage('Telefone removido.', True);
end;

{ Botao "Adicionar Telefone": valida, garante contato salvo, insere telefone }
procedure TFormContato.BtnAdicionarTelefoneClick(Sender: TObject);
var
  Numero: string;
begin
  Numero := Trim(EditTelefone.Text);

  { Validacao: campo nao pode estar vazio }
  if Numero = '' then
  begin
    ShowMessage('Digite o numero do telefone.', True, 3000, mtWarning);
    Exit;
  end;

  { Se o contato ainda nao foi salvo, salva agora (auto-save) }
  if not GarantirContatoSalvo then
    Exit;

  { INSERT direto no MySQL }
  DM.InserirTelefoneSQL(FContatoID, Numero);

  { Grava log da adicao do telefone }
  GravarLog(tlTelefoneAdd,
    Format('Telefone adicionado - ContatoID: %d | Numero: %s', [FContatoID, Numero]));

  { Atualiza grid e limpa campo }
  RefreshTelefones;
  EditTelefone.Text := '';

  ShowMessage('Telefone adicionado.', True);
end;

{ Botao "Remover Telefone" (nao usado na pratica - grid tem lixeira) }
procedure TFormContato.BtnRemoverTelefoneClick(Sender: TObject);
begin
  ExcluirTelefoneDaLinha;
end;

{ ========================================================================== }
{  SALVAR / CANCELAR / EXCLUIR CONTATO                                        }
{ ========================================================================== }

{ Botao "Salvar": grava contato no MySQL e volta para Home.
  Trata dois cenarios: novo (INSERT) e edicao (UPDATE). }
procedure TFormContato.BtnSalvarClick(Sender: TObject);
begin
  { Validacao obrigatoria: nome nao pode estar vazio }
  if Trim(DBEditNome.Text) = '' then
  begin
    ShowMessage('O campo Nome e obrigatorio.', True, 4000, mtWarning);
    Exit;
  end;

  if FContatoID = 0 then
  begin
    { === MODO CRIACAO: salva e obtem novo ID === }
    FContatoID := DM.SalvarContatoRetornarID;
    if FContatoID = 0 then
    begin
      ShowMessage('Erro ao salvar o contato.', True, 5000, mtError);
      Exit;
    end;
    { Log de criacao com dados do contato }
    GravarLogCriacao(FContatoID, Trim(DBEditNome.Text),
      StrToIntDef(Trim(DBEditIdade.Text), 0));
  end
  else
  begin
    { === MODO EDICAO: faz Post + ApplyUpdates === }
    if not DM.SalvarContato then
    begin
      ShowMessage('Erro ao salvar o contato.', True, 5000, mtError);
      Exit;
    end;
    { Log de edicao com dados atualizados }
    GravarLogEdicao(FContatoID, Trim(DBEditNome.Text),
      StrToIntDef(Trim(DBEditIdade.Text), 0));
  end;

  ShowMessage('Contato salvo com sucesso!', True);
  VoltarParaHome;
end;

{ Botao "Voltar": cancela alteracoes pendentes e volta ao Home.
  Cancel descarta mudancas que estavam em memoria (CachedUpdates). }
procedure TFormContato.BtnCancelarClick(Sender: TObject);
begin
  if DM.QryContato.Active and (DM.QryContato.State in [dsEdit, dsInsert]) then
    DM.QryContato.Cancel; { Descarta alteracoes nao salvas }

  VoltarParaHome;
end;

{ Botao "Excluir Contato": pede confirmacao via MessageDlg, coleta dados
  para log detalhado, exclui contato (CASCADE remove telefones) e volta. }
procedure TFormContato.BtnExcluirContatoClick(Sender: TObject);
var
  Nome, Telefones: string;
  Idade: Integer;
begin
  if FContatoID = 0 then Exit; { Nao ha contato para excluir }

  { Coleta nome para exibir no dialogo de confirmacao }
  Nome := '';
  if DM.QryContato.Active and not DM.QryContato.IsEmpty then
    Nome := DM.QryContato.FieldByName('NOME').AsString;

  { Dialogo de confirmacao (SweetAlert2 no navegador via D2Bridge).
    Bloqueia execucao ate o usuario responder Sim ou Nao. }
  if MessageDlg('Deseja realmente excluir o contato "' + Nome + '"?',
    TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0) <> mrYes then
    Exit; { Usuario cancelou }

  { Coleta dados completos ANTES de excluir (para o log) }
  Idade := 0;
  Telefones := '';
  if DM.QryContato.Active and not DM.QryContato.IsEmpty then
    Idade := DM.QryContato.FieldByName('IDADE').AsInteger;

  { Monta string com todos os telefones separados por virgula }
  if DM.QryTelefones.Active then
  begin
    DM.QryTelefones.First; { Move para o primeiro registro }
    while not DM.QryTelefones.Eof do { Percorre ate o fim }
    begin
      if Telefones <> '' then
        Telefones := Telefones + ', ';
      Telefones := Telefones + DM.QryTelefones.FieldByName('NUMERO').AsString;
      DM.QryTelefones.Next; { Avanca para o proximo }
    end;
  end;

  { Grava log detalhado com todos os dados que serao perdidos }
  GravarLogExclusao(FContatoID, Nome, Idade, Telefones);

  { DELETE no MySQL - CASCADE remove telefones automaticamente }
  DM.ExcluirContato(FContatoID);

  ShowMessage('Contato "' + Nome + '" excluido com sucesso.', True);
  VoltarParaHome;
end;

{ ========================================================================== }
{  NAVEGACAO - Retorno para a tela Home                                       }
{                                                                             }
{  Limpa a sessao (ID e nome selecionado), recarrega a pesquisa              }
{  de contatos e exibe o FormHome.                                            }
{ ========================================================================== }

procedure TFormContato.VoltarParaHome;
begin
  { Reseta dados de sessao para que a proxima visita ao FormContato
    nao carregue o contato anterior }
  AppSession.ContatoSelecionadoID := 0;
  FContatoID := 0;

  { Recarrega todos os contatos (sem filtro) para o grid do Home }
  DM.PesquisarContatos('', '');

  { Exibe o FormHome no navegador do usuario }
  FormHome.Show;
end;

end.
