{ ============================================================================
  Unit_FormHome.pas  -  Tela Principal (Home)

  Listagem de contatos com pesquisa unificada (nome ou telefone).
  Cada linha do grid possui botoes Editar e Excluir.
  ============================================================================ }
unit Unit_FormHome;

interface

uses
  System.SysUtils, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.Grids, Vcl.DBGrids,
  Data.DB,
  D2Bridge.Forms;

type
  TFormHome = class(TD2BridgeForm)
    EditPesquisa: TEdit;
    BtnPesquisar: TButton;
    BtnNovo: TButton;
    DBGridContatos: TDBGrid;

    procedure BtnPesquisarClick(Sender: TObject);
    procedure BtnNovoClick(Sender: TObject);
  private
    LabelTitulo: TLabel;
    procedure DoPageLoaded(Sender: TObject);
    procedure ConectarDataSources;
    procedure CarregarContatos;
    procedure EditarContato;
    procedure ExcluirContato;
  protected
    procedure ExportD2Bridge; override;
    procedure InitControlsD2Bridge(const PrismControl: TPrismControl); override;
  end;

function FormHome: TFormHome;

implementation

{$R *.dfm}

uses
  Unit_DM, Unit_Log, Unit_ServerController, Unit_FormContato;

function FormHome: TFormHome;
begin
  Result := TFormHome(TFormHome.GetInstance);
end;

{ ========================================================================== }
{  EXPORT D2BRIDGE - Layout                                                   }
{ ========================================================================== }
procedure TFormHome.ExportD2Bridge;
begin
  inherited;
  Title := 'Agenda de Contatos';

  Self.OnPageLoaded := DoPageLoaded;

  { Label do titulo (criado dinamicamente para H2) }
  LabelTitulo := TLabel.Create(Self);
  LabelTitulo.Caption := 'Agenda de Contatos';

  { Conectar DataSource ANTES de adicionar o grid ao D2Bridge }
  DM.EnsureConnected;
  DM.PesquisarContatos('', '');
  ConectarDataSources;

  with D2Bridge.Items.Add do
  begin
   { Container com largura maxima de 1600px centralizado }
   with HTMLDIV('', '', '', 'max-width:1600px; margin:0 auto; width:100%').Items.Add do
   begin
    { H2 - Titulo da pagina }
    with HTMLDIV('mb-3', '', '', '', 'h2').Items.Add do
      VCLObj(LabelTitulo);

    { Cabecalho: pesquisa a esquerda, novo contato a direita }
    with Row.Items.Add do
    begin
      with FormGroup('', CSSClass.Col.colsize7) do
        AddVCLObj(EditPesquisa);
      ColAuto.Add.VCLObj(BtnPesquisar, CSSClass.Button.search);
      ColAuto.Add.VCLObj(BtnNovo, CSSClass.Button.add);
    end;

    { Grid de contatos - ocupa toda a largura, com gap acima }
    with HTMLDIV('mt-3').Items.Add do
      VCLObj(DBGridContatos);
   end;
  end;
end;

{ ========================================================================== }
{  INIT CONTROLS - Colunas do grid com botoes Editar/Excluir por linha        }
{ ========================================================================== }
procedure TFormHome.InitControlsD2Bridge(const PrismControl: TPrismControl);
begin
  inherited;

  if PrismControl.VCLComponent = DBGridContatos then
  begin
    with PrismControl.AsDBGrid do
    begin
      { Ajustar largura das colunas de dados }
      if Columns.ColumnByDataField('ID') <> nil then
        Columns.ColumnByDataField('ID').Visible := False;
      if Columns.ColumnByDataField('NOME') <> nil then
        Columns.ColumnByDataField('NOME').Width := 200;
      if Columns.ColumnByDataField('IDADE') <> nil then
        Columns.ColumnByDataField('IDADE').Width := 60;
      if Columns.ColumnByDataField('TELEFONES') <> nil then
        Columns.ColumnByDataField('TELEFONES').Width := 250;

      { Coluna de acoes com botoes Editar/Excluir }
      with Columns.Add do
      begin
        Title := 'Acoes';
        Width := 100;

        with Buttons.Add do
        begin
          ButtonModel := TButtonModel.Edit;
          ClickProc := procedure
          begin
            EditarContato;
          end;
        end;

        with Buttons.Add do
        begin
          ButtonModel := TButtonModel.Delete;
          ClickProc := procedure
          begin
            ExcluirContato;
          end;
        end;
      end;
    end;
  end;
end;

{ ========================================================================== }
{  EVENTOS                                                                    }
{ ========================================================================== }

procedure TFormHome.DoPageLoaded(Sender: TObject);
begin
  CarregarContatos;
end;

procedure TFormHome.ConectarDataSources;
begin
  DBGridContatos.DataSource := DM.DSPesquisa;
end;

procedure TFormHome.BtnPesquisarClick(Sender: TObject);
begin
  CarregarContatos;
end;

procedure TFormHome.BtnNovoClick(Sender: TObject);
begin
  AppSession.ContatoSelecionadoID := 0;
  AppSession.ContatoSelecionadoNome := '';

  if FormContato = nil then
    TFormContato.CreateInstance;
  FormContato.Show;
end;

{ ========================================================================== }
{  METODOS PRIVADOS                                                           }
{ ========================================================================== }

procedure TFormHome.CarregarContatos;
begin
  DM.PesquisarContatos(Trim(EditPesquisa.Text), Trim(EditPesquisa.Text));
end;

procedure TFormHome.EditarContato;
var
  ID: Integer;
begin
  if DM.QryPesquisa.IsEmpty then Exit;

  ID := DM.QryPesquisa.FieldByName('ID').AsInteger;

  AppSession.ContatoSelecionadoID := ID;
  AppSession.ContatoSelecionadoNome := DM.QryPesquisa.FieldByName('NOME').AsString;

  if FormContato = nil then
    TFormContato.CreateInstance;
  FormContato.Show;
end;

procedure TFormHome.ExcluirContato;
var
  ID, Idade: Integer;
  Nome, Telefones: string;
begin
  if DM.QryPesquisa.IsEmpty then Exit;

  ID        := DM.QryPesquisa.FieldByName('ID').AsInteger;
  Nome      := DM.QryPesquisa.FieldByName('NOME').AsString;
  Idade     := DM.QryPesquisa.FieldByName('IDADE').AsInteger;
  Telefones := DM.QryPesquisa.FieldByName('TELEFONES').AsString;

  if MessageDlg('Deseja realmente excluir o contato "' + Nome + '"?',
    TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0) <> mrYes then
    Exit;

  GravarLogExclusao(ID, Nome, Idade, Telefones);
  DM.ExcluirContato(ID);
  ShowMessage('Contato "' + Nome + '" excluido com sucesso.', True);
  CarregarContatos;
end;

end.
