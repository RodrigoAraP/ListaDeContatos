{ ============================================================================
  Unit_FormHome.pas  -  Tela Principal (Home)

  Listagem de contatos com pesquisa unificada (nome ou telefone).
  Cada linha do grid possui botoes Editar e Excluir.

  Conceitos-chave:
  - TD2BridgeForm: formulario web do D2Bridge (herda de TForm VCL)
  - ExportD2Bridge: executado UMA vez por sessao - monta o layout HTML
  - InitControlsD2Bridge: configura cada componente apos renderizacao
  - DoPageLoaded: disparado a cada navegacao - recarrega dados
  - GetInstance/CreateInstance: padrao Singleton por sessao D2Bridge
  - ClickProc: lambda (procedure anonima) executada ao clicar botao
  - HTMLDIV: injeta HTML customizado no layout Bootstrap 5
  - MessageDlg: no D2Bridge renderiza um SweetAlert2 (pop-up bonito)
  ============================================================================ }
unit Unit_FormHome;

interface

uses
  System.SysUtils, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.Grids, Vcl.DBGrids,
  Data.DB,
  D2Bridge.Forms; { Classe base TD2BridgeForm e tipos auxiliares }

type
  { TFormHome herda de TD2BridgeForm.
    O D2Bridge converte este formulario VCL em pagina web automaticamente. }
  TFormHome = class(TD2BridgeForm)

    { Componentes criados no DFM (design-time) }
    EditPesquisa: TEdit;       { Campo de busca por nome ou telefone }
    BtnPesquisar: TButton;     { Botao lupa - dispara busca }
    BtnNovo: TButton;          { Botao "+ Novo Contato" }
    DBGridContatos: TDBGrid;   { Grid conectado ao QryPesquisa via DSPesquisa }

    { Eventos definidos no DFM (OnClick dos botoes) }
    procedure BtnPesquisarClick(Sender: TObject);
    procedure BtnNovoClick(Sender: TObject);
  private
    { Label criado em runtime (nao no DFM) para ser renderizado como H2 }
    LabelTitulo: TLabel;

    { Callback registrado em OnPageLoaded - executado a cada navegacao }
    procedure DoPageLoaded(Sender: TObject);

    { Liga os componentes visuais aos DataSources do DataModule }
    procedure ConectarDataSources;

    { Executa a pesquisa com filtro do EditPesquisa }
    procedure CarregarContatos;

    { Abre tela de edicao para o contato selecionado no grid }
    procedure EditarContato;

    { Exclui contato com dialogo de confirmacao (MessageDlg) }
    procedure ExcluirContato;
  protected
    { ExportD2Bridge: executado UMA VEZ quando o form e exibido pela primeira vez.
      Define o layout HTML/Bootstrap: quais componentes aparecem, em que ordem,
      com que classes CSS. Equivale ao "design visual" da pagina web. }
    procedure ExportD2Bridge; override;

    { InitControlsD2Bridge: chamado para CADA componente apos renderizacao.
      Permite customizar colunas do grid, adicionar botoes por linha, etc.
      O parametro PrismControl identifica qual componente esta sendo configurado. }
    procedure InitControlsD2Bridge(const PrismControl: TPrismControl); override;
  end;

{ Funcao global - retorna a instancia UNICA deste form na sessao atual.
  Se a instancia nao existir, retorna nil.
  Padrao Singleton por sessao D2Bridge. }
function FormHome: TFormHome;

implementation

{ Inclui o arquivo DFM com propriedades dos componentes (posicao, tamanho etc.) }
{$R *.dfm}

uses
  Unit_DM,               { Acesso ao DataModule (banco de dados) }
  Unit_Log,              { GravarLogExclusao }
  Unit_ServerController, { AppSession - sessao do usuario }
  Unit_FormContato;      { FormContato - tela de edicao }

{ Obtem a instancia do FormHome na sessao atual via D2Bridge.
  GetInstance busca no cache de formularios da sessao. }
function FormHome: TFormHome;
begin
  Result := TFormHome(TFormHome.GetInstance);
end;

{ ========================================================================== }
{  EXPORT D2BRIDGE - Layout da pagina Web                                     }
{                                                                             }
{  Este metodo monta a ESTRUTURA HTML da pagina usando a API fluente:         }
{  D2Bridge.Items.Add -> HTMLDIV -> Row -> FormGroup -> VCLObj               }
{  O D2Bridge converte essa arvore em Bootstrap 5 automaticamente.            }
{ ========================================================================== }
procedure TFormHome.ExportD2Bridge;
begin
  inherited; { Sempre chamar inherited para inicializacao do framework }
  Title := 'Agenda de Contatos'; { Define o <title> da pagina HTML }

  { Registra callback DoPageLoaded para recarregar dados a cada navegacao.
    OnPageLoaded e disparado TODA vez que o usuario acessa esta pagina,
    diferente do ExportD2Bridge que roda so uma vez. }
  Self.OnPageLoaded := DoPageLoaded;

  { Cria um TLabel em runtime para ser renderizado como tag <h2>.
    Self como Owner garante que sera liberado junto com o form. }
  LabelTitulo := TLabel.Create(Self);
  LabelTitulo.Caption := 'Agenda de Contatos';

  { Conectar DataSource ANTES de adicionar o grid ao D2Bridge.
    Isso garante que o grid saiba quais colunas renderizar. }
  DM.EnsureConnected;
  DM.PesquisarContatos('', ''); { Carrega todos os contatos (sem filtro) }
  ConectarDataSources;

  { Inicio da arvore de layout D2Bridge }
  with D2Bridge.Items.Add do
  begin
   { Container principal com largura maxima de 1600px, centralizado.
     HTMLDIV(CSSClass, ID, Name, Style, TagHTML) gera uma <div>.
     margin:0 auto centraliza horizontalmente. }
   with HTMLDIV('', '', '', 'max-width:1600px; margin:0 auto; width:100%').Items.Add do
   begin
    { Titulo H2 - HTMLDIV com 5o parametro 'h2' gera tag <h2>.
      mb-3 = margin-bottom do Bootstrap (espacamento abaixo). }
    with HTMLDIV('mb-3', '', '', '', 'h2').Items.Add do
      VCLObj(LabelTitulo); { VCLObj conecta o TLabel ao HTML }

    { Linha de pesquisa usando grid Bootstrap (Row + Col).
      Row cria uma <div class="row">. }
    with Row.Items.Add do
    begin
      { FormGroup cria <div class="col-7"> com label opcional.
        colsize7 = ocupa 7 das 12 colunas do Bootstrap. }
      with FormGroup('', CSSClass.Col.colsize7) do
        AddVCLObj(EditPesquisa); { Renderiza como <input type="text"> }

      { ColAuto = <div class="col-auto"> - ocupa so o espaco necessario.
        CSSClass.Button.search = icone de lupa no botao. }
      ColAuto.Add.VCLObj(BtnPesquisar, CSSClass.Button.search);

      { CSSClass.Button.add = icone "+" no botao. }
      ColAuto.Add.VCLObj(BtnNovo, CSSClass.Button.add);
    end;

    { Grid de contatos numa div com mt-3 (margin-top = espacamento acima) }
    with HTMLDIV('mt-3').Items.Add do
      VCLObj(DBGridContatos); { Renderiza como <table> Bootstrap }
   end;
  end;
end;

{ ========================================================================== }
{  INIT CONTROLS - Customizacao das colunas do grid                           }
{                                                                             }
{  InitControlsD2Bridge e chamado para CADA componente VCL do formulario.      }
{  Verificamos se o componente atual e o DBGridContatos, e se for,            }
{  ajustamos larguras de coluna e adicionamos botoes de acao por linha.       }
{ ========================================================================== }
procedure TFormHome.InitControlsD2Bridge(const PrismControl: TPrismControl);
begin
  inherited;

  { Verifica se o componente sendo configurado e o grid de contatos }
  if PrismControl.VCLComponent = DBGridContatos then
  begin
    { AsDBGrid converte PrismControl para manipular colunas }
    with PrismControl.AsDBGrid do
    begin
      { Esconde coluna ID - o usuario nao precisa ver o ID interno }
      if Columns.ColumnByDataField('ID') <> nil then
        Columns.ColumnByDataField('ID').Visible := False;

      { Define larguras fixas em pixels para as colunas de dados }
      if Columns.ColumnByDataField('NOME') <> nil then
        Columns.ColumnByDataField('NOME').Width := 200;
      if Columns.ColumnByDataField('IDADE') <> nil then
        Columns.ColumnByDataField('IDADE').Width := 60;
      if Columns.ColumnByDataField('TELEFONES') <> nil then
        Columns.ColumnByDataField('TELEFONES').Width := 250;

      { Adiciona coluna "Acoes" com botoes Editar e Excluir.
        Esta coluna nao tem DataField - e puramente visual. }
      with Columns.Add do
      begin
        Title := 'Acoes';
        Width := 100;

        { Botao Editar - icone de lapis (TButtonModel.Edit).
          ClickProc recebe uma procedure anonima (lambda) que sera
          executada quando o usuario clicar nesse botao na linha. }
        with Buttons.Add do
        begin
          ButtonModel := TButtonModel.Edit;
          ClickProc := procedure
          begin
            EditarContato; { Navega para tela de edicao }
          end;
        end;

        { Botao Excluir - icone de lixeira (TButtonModel.Delete).
          Cada linha do grid tera seu proprio botao de exclusao. }
        with Buttons.Add do
        begin
          ButtonModel := TButtonModel.Delete;
          ClickProc := procedure
          begin
            ExcluirContato; { Pede confirmacao e exclui }
          end;
        end;
      end;
    end;
  end;
end;

{ ========================================================================== }
{  EVENTOS DE PAGINA E BOTOES                                                 }
{ ========================================================================== }

{ DoPageLoaded: executado TODA vez que a pagina Home e carregada/navegada.
  Diferente do ExportD2Bridge (que roda so uma vez), este atualiza os dados. }
procedure TFormHome.DoPageLoaded(Sender: TObject);
begin
  CarregarContatos; { Recarrega grid com dados atualizados do MySQL }
end;

{ Liga o DBGrid ao DataSource do DataModule.
  DSPesquisa -> QryPesquisa (query com GROUP_CONCAT). }
procedure TFormHome.ConectarDataSources;
begin
  DBGridContatos.DataSource := DM.DSPesquisa;
end;

{ Botao de busca: recarrega grid com filtro digitado pelo usuario }
procedure TFormHome.BtnPesquisarClick(Sender: TObject);
begin
  CarregarContatos;
end;

{ Botao "Novo Contato": prepara sessao para criacao e navega ao FormContato }
procedure TFormHome.BtnNovoClick(Sender: TObject);
begin
  { ID = 0 indica para o FormContato que e um NOVO contato }
  AppSession.ContatoSelecionadoID := 0;
  AppSession.ContatoSelecionadoNome := '';

  { CreateInstance cria o formulario se ele nao existir na sessao.
    Show exibe a pagina no navegador do usuario. }
  if FormContato = nil then
    TFormContato.CreateInstance;
  FormContato.Show;
end;

{ ========================================================================== }
{  METODOS PRIVADOS                                                           }
{ ========================================================================== }

{ Executa pesquisa enviando o mesmo texto para filtro de nome e telefone.
  O DataModule constroi SQL com LIKE :pFiltro em ambos os campos. }
procedure TFormHome.CarregarContatos;
begin
  DM.PesquisarContatos(Trim(EditPesquisa.Text), Trim(EditPesquisa.Text));
end;

{ Editar Contato: le o ID da linha atual do grid e navega ao FormContato.
  Grava ID e Nome na sessao para que o FormContato saiba qual contato abrir. }
procedure TFormHome.EditarContato;
var
  ID: Integer;
begin
  if DM.QryPesquisa.IsEmpty then Exit; { Nada selecionado }

  { FieldByName acessa o campo pelo nome da coluna SQL }
  ID := DM.QryPesquisa.FieldByName('ID').AsInteger;

  { Grava na sessao para transferir dados entre telas }
  AppSession.ContatoSelecionadoID := ID;
  AppSession.ContatoSelecionadoNome := DM.QryPesquisa.FieldByName('NOME').AsString;

  { Cria ou obtem a instancia do FormContato e exibe }
  if FormContato = nil then
    TFormContato.CreateInstance;
  FormContato.Show;
end;

{ Excluir Contato: le dados da linha, pede confirmacao via MessageDlg,
  grava log detalhado e remove do banco.
  ON DELETE CASCADE remove automaticamente os telefones vinculados. }
procedure TFormHome.ExcluirContato;
var
  ID, Idade: Integer;
  Nome, Telefones: string;
begin
  if DM.QryPesquisa.IsEmpty then Exit;

  { Coleta todos os dados ANTES de excluir (para gravar no log) }
  ID        := DM.QryPesquisa.FieldByName('ID').AsInteger;
  Nome      := DM.QryPesquisa.FieldByName('NOME').AsString;
  Idade     := DM.QryPesquisa.FieldByName('IDADE').AsInteger;
  Telefones := DM.QryPesquisa.FieldByName('TELEFONES').AsString;

  { MessageDlg no D2Bridge renderiza como SweetAlert2 (pop-up JavaScript).
    mtConfirmation = icone de pergunta, mbYes/mbNo = botoes Sim/Nao.
    O framework bloqueia a execucao ate o usuario responder. }
  if MessageDlg('Deseja realmente excluir o contato "' + Nome + '"?',
    TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0) <> mrYes then
    Exit; { Usuario clicou "Nao" - cancela exclusao }

  { Grava log com dados completos ANTES de excluir do banco }
  GravarLogExclusao(ID, Nome, Idade, Telefones);

  { Remove do MySQL - CASCADE exclui telefones automaticamente }
  DM.ExcluirContato(ID);

  { Feedback visual e atualiza o grid }
  ShowMessage('Contato "' + Nome + '" excluido com sucesso.', True);
  CarregarContatos;
end;

end.
