{ ============================================================================
  Unit_DM.pas  -  DataModule (Acesso a Dados)

  Centraliza toda comunicacao com o MySQL via FireDAC.
  Cada sessao D2Bridge recebe sua propria instancia, garantindo
  isolamento entre usuarios simultaneos.
  ============================================================================ }
unit Unit_DM;

interface

uses
  System.SysUtils, System.Classes, System.IniFiles,
  Data.DB,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error,
  FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async,
  FireDAC.UI.Intf, FireDAC.VCLUI.Wait, FireDAC.Stan.Param,
  FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt,
  FireDAC.Phys.Intf, FireDAC.Phys,
  FireDAC.Phys.MySQL, FireDAC.Phys.MySQLDef,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client, FireDAC.Comp.UI,
  D2Bridge.Instance;

type
  TDM = class(TDataModule)
    FDConnection: TFDConnection;
    FDGUIxWaitCursor: TFDGUIxWaitCursor;

    { Query de pesquisa - lista contatos com telefones }
    QryPesquisa: TFDQuery;
    DSPesquisa: TDataSource;

    { Query de edição do contato individual }
    QryContato: TFDQuery;
    DSContato: TDataSource;

    { Query de telefones do contato selecionado }
    QryTelefones: TFDQuery;
    DSTelefones: TDataSource;

    { Query auxiliar para execução de comandos }
    QryExec: TFDQuery;

    procedure DataModuleCreate(Sender: TObject);
  private
    FConectado: Boolean;
    procedure ConfigurarConexao;
  public
    procedure EnsureConnected;
    procedure PesquisarContatos(const AFiltroNome, AFiltroTelefone: string);
    procedure AbrirContato(AID: Integer);
    procedure AbrirTelefones(AIDContato: Integer);
    procedure InserirContato;
    function  SalvarContato: Boolean;
    function  SalvarContatoRetornarID: Integer;
    procedure ExcluirContato(AID: Integer);
    procedure InserirTelefoneSQL(AIDContato: Integer; const ANumero: string);
    procedure ExcluirTelefonePorID(AIDTelefone: Integer);
  end;

{ Função global - retorna instância do DM da sessão atual }
function DM: TDM;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

function DM: TDM;
begin
  Result := TDM(D2BridgeInstance.GetInstance(TDM));
end;

{ -------------------------------------------------------------------------- }
procedure TDM.DataModuleCreate(Sender: TObject);
begin
  FConectado := False;
  ConfigurarConexao; { apenas configura parametros, nao conecta }
end;

{ -------------------------------------------------------------------------- }
procedure TDM.ConfigurarConexao;
var
  Ini: TIniFile;
  IniPath: string;
begin
  IniPath := ExtractFilePath(ParamStr(0)) + 'Config.ini';

  if not FileExists(IniPath) then
    raise Exception.Create('Config.ini nao encontrado em: ' + IniPath);

  Ini := TIniFile.Create(IniPath);
  try
    FDConnection.Params.Clear;
    FDConnection.Params.DriverID  := 'MySQL';
    FDConnection.Params.Database  := Ini.ReadString('Database', 'Database', 'agenda_contatos');
    FDConnection.Params.UserName  := Ini.ReadString('Database', 'User', 'root');
    FDConnection.Params.Password  := Ini.ReadString('Database', 'Password', '');
    FDConnection.Params.Add('Server='   + Ini.ReadString('Database', 'Host', 'localhost'));
    FDConnection.Params.Add('Port='     + Ini.ReadString('Database', 'Port', '3306'));
    FDConnection.Params.Add('CharacterSet=utf8mb4');
  finally
    Ini.Free;
  end;
end;

{ -------------------------------------------------------------------------- }
procedure TDM.EnsureConnected;
begin
  if not FConectado then
  begin
    FDConnection.Connected := True;
    FConectado := True;
  end;
end;

{ -------------------------------------------------------------------------- }
{  Pesquisa unificada: busca por nome OU telefone num unico campo.           }
{  Usa sub-select com GROUP_CONCAT para exibir todos os telefones por linha. }
{ -------------------------------------------------------------------------- }
procedure TDM.PesquisarContatos(const AFiltroNome, AFiltroTelefone: string);
var
  SQL, Filtro: string;
begin
  EnsureConnected;
  QryPesquisa.Close;

  SQL :=
    'SELECT DISTINCT c.ID, c.NOME, c.IDADE, ' +
    '  (SELECT GROUP_CONCAT(t2.NUMERO SEPARATOR '', '') ' +
    '   FROM telefone t2 WHERE t2.IDCONTATO = c.ID) AS TELEFONES ' +
    'FROM contato c ' +
    'LEFT JOIN telefone t ON t.IDCONTATO = c.ID ';

  { Campo unico de pesquisa: nome e telefone recebem o mesmo valor }
  Filtro := Trim(AFiltroNome);
  if Filtro <> '' then
    SQL := SQL + 'WHERE (c.NOME LIKE :pFiltro OR t.NUMERO LIKE :pFiltro) ';

  SQL := SQL + 'ORDER BY c.NOME';

  QryPesquisa.SQL.Text := SQL;

  if Filtro <> '' then
    QryPesquisa.ParamByName('pFiltro').AsString := '%' + Filtro + '%';

  QryPesquisa.Open;
end;

{ -------------------------------------------------------------------------- }
procedure TDM.AbrirContato(AID: Integer);
begin
  EnsureConnected;
  QryContato.Close;
  QryContato.SQL.Text := 'SELECT * FROM contato WHERE ID = :pID';
  QryContato.ParamByName('pID').AsInteger := AID;
  QryContato.Open;
end;

{ -------------------------------------------------------------------------- }
procedure TDM.AbrirTelefones(AIDContato: Integer);
begin
  EnsureConnected;
  QryTelefones.Close;
  QryTelefones.SQL.Text := 'SELECT ID, NUMERO FROM telefone WHERE IDCONTATO = :pIDContato ORDER BY ID';
  QryTelefones.ParamByName('pIDContato').AsInteger := AIDContato;
  QryTelefones.Open;
end;

{ -------------------------------------------------------------------------- }
procedure TDM.InserirContato;
begin
  EnsureConnected;
  QryContato.Close;
  QryContato.SQL.Text := 'SELECT * FROM contato WHERE 1=0';
  QryContato.Open;
  QryContato.Append;
end;

{ -------------------------------------------------------------------------- }
function TDM.SalvarContato: Boolean;
begin
  Result := False;

  if QryContato.State in [dsEdit, dsInsert] then
  begin
    { Validação básica }
    if Trim(QryContato.FieldByName('NOME').AsString) = '' then
      Exit;

    QryContato.Post;
    QryContato.ApplyUpdates(0);
    Result := True;
  end;
end;

{ -------------------------------------------------------------------------- }
procedure TDM.ExcluirContato(AID: Integer);
begin
  EnsureConnected;
  QryExec.SQL.Text := 'DELETE FROM contato WHERE ID = :pID';
  QryExec.ParamByName('pID').AsInteger := AID;
  QryExec.ExecSQL;
end;

{ -------------------------------------------------------------------------- }
{  Salva contato novo e retorna o ID gerado (LAST_INSERT_ID).               }
{  Em caso de edicao (ID ja existe), retorna o ID corrente.                 }
{ -------------------------------------------------------------------------- }
function TDM.SalvarContatoRetornarID: Integer;
begin
  Result := 0;
  if not SalvarContato then
    Exit;

  { Buscar o ID gerado via LAST_INSERT_ID }
  QryExec.SQL.Text := 'SELECT LAST_INSERT_ID() AS NOVO_ID';
  QryExec.Open;
  try
    if not QryExec.IsEmpty then
      Result := QryExec.FieldByName('NOVO_ID').AsInteger;
  finally
    QryExec.Close;
  end;

  { Se LAST_INSERT_ID nao retornou (edicao), pegar do QryContato }
  if (Result = 0) and QryContato.Active and not QryContato.IsEmpty then
    Result := QryContato.FieldByName('ID').AsInteger;
end;

{ -------------------------------------------------------------------------- }
procedure TDM.InserirTelefoneSQL(AIDContato: Integer; const ANumero: string);
begin
  EnsureConnected;
  QryExec.SQL.Text := 'INSERT INTO telefone (IDCONTATO, NUMERO) VALUES (:pID, :pNumero)';
  QryExec.ParamByName('pID').AsInteger := AIDContato;
  QryExec.ParamByName('pNumero').AsString := ANumero;
  QryExec.ExecSQL;
end;

{ -------------------------------------------------------------------------- }
procedure TDM.ExcluirTelefonePorID(AIDTelefone: Integer);
begin
  EnsureConnected;
  QryExec.SQL.Text := 'DELETE FROM telefone WHERE ID = :pID';
  QryExec.ParamByName('pID').AsInteger := AIDTelefone;
  QryExec.ExecSQL;
end;

end.
