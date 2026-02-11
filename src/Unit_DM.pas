{ ============================================================================
  Unit_DM.pas  -  DataModule (Acesso a Dados)

  Centraliza toda comunicacao com o MySQL via FireDAC.
  Cada sessao D2Bridge recebe sua propria instancia, garantindo
  isolamento entre usuarios simultaneos.

  Conceitos-chave:
  - TDataModule: container de componentes nao-visuais (queries, conexao)
  - FireDAC: framework de acesso a dados do Delphi (suporta MySQL, PostgreSQL, etc.)
  - TFDQuery: componente para executar SQL (SELECT, INSERT, DELETE...)
  - TDataSource: ponte entre um TFDQuery e componentes visuais (DBEdit, DBGrid)
  - CachedUpdates: alteracoes ficam em memoria ate chamar ApplyUpdates
  - Parametros (:param): protecao contra SQL Injection
  ============================================================================ }
unit Unit_DM;

interface

uses
  System.SysUtils, System.Classes, System.IniFiles,
  Data.DB,
  { FireDAC - framework de acesso a dados do Delphi }
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error,
  FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async,
  FireDAC.UI.Intf, FireDAC.VCLUI.Wait, FireDAC.Stan.Param,
  FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt,
  FireDAC.Phys.Intf, FireDAC.Phys,
  FireDAC.Phys.MySQL, FireDAC.Phys.MySQLDef, { Driver MySQL especifico }
  FireDAC.Comp.DataSet, FireDAC.Comp.Client, FireDAC.Comp.UI,
  D2Bridge.Instance; { Para funcao DM() - obter instancia da sessao }

type
  TDM = class(TDataModule)
    { Conexao com o banco MySQL - configurada via Config.ini }
    FDConnection: TFDConnection;

    { Componente que exibe cursor de espera durante operacoes do FireDAC }
    FDGUIxWaitCursor: TFDGUIxWaitCursor;

    { === PESQUISA (tela Home) === }
    { Query que lista contatos com telefones agrupados por GROUP_CONCAT }
    QryPesquisa: TFDQuery;
    { DataSource conecta QryPesquisa ao DBGrid da tela Home }
    DSPesquisa: TDataSource;

    { === EDICAO DE CONTATO (tela Contato) === }
    { Query de um contato individual - usa CachedUpdates para editar em memoria }
    QryContato: TFDQuery;
    { DataSource conecta QryContato aos DBEdits (Nome, Idade) }
    DSContato: TDataSource;

    { === TELEFONES DO CONTATO === }
    { Query que lista telefones de um contato especifico }
    QryTelefones: TFDQuery;
    { DataSource conecta QryTelefones ao DBGrid de telefones }
    DSTelefones: TDataSource;

    { === QUERY AUXILIAR === }
    { Usada para INSERT, DELETE e LAST_INSERT_ID (nao tem DataSource) }
    QryExec: TFDQuery;

    { Evento disparado automaticamente ao criar o DataModule }
    procedure DataModuleCreate(Sender: TObject);
  private
    { Flag para controle de conexao lazy (conecta apenas quando precisar) }
    FConectado: Boolean;

    { Le Config.ini e configura os parametros da conexao (sem conectar) }
    procedure ConfigurarConexao;
  public
    { Conecta ao MySQL na primeira chamada (padrao Lazy Initialization) }
    procedure EnsureConnected;

    { Busca contatos por nome ou telefone - campo unico de pesquisa }
    procedure PesquisarContatos(const AFiltroNome, AFiltroTelefone: string);

    { Abre um contato especifico pelo ID para edicao }
    procedure AbrirContato(AID: Integer);

    { Abre a lista de telefones de um contato }
    procedure AbrirTelefones(AIDContato: Integer);

    { Prepara QryContato para inserir um novo contato (Append) }
    procedure InserirContato;

    { Grava as alteracoes do contato no banco (Post + ApplyUpdates) }
    function  SalvarContato: Boolean;

    { Salva contato novo e retorna o ID gerado (LAST_INSERT_ID) }
    function  SalvarContatoRetornarID: Integer;

    { Exclui um contato pelo ID (CASCADE remove telefones) }
    procedure ExcluirContato(AID: Integer);

    { Insere um telefone diretamente via SQL (INSERT INTO) }
    procedure InserirTelefoneSQL(AIDContato: Integer; const ANumero: string);

    { Remove um telefone pelo seu ID }
    procedure ExcluirTelefonePorID(AIDTelefone: Integer);
  end;

{ Funcao global - retorna a instancia do DM da sessao ATUAL.
  Como cada usuario tem seu proprio DM, isso garante isolamento. }
function DM: TDM;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{ Inclui o arquivo DFM que contem as propriedades dos componentes visuais }
{$R *.dfm}

{ Obtem o DataModule da sessao atual via D2Bridge }
function DM: TDM;
begin
  Result := TDM(D2BridgeInstance.GetInstance(TDM));
end;

{ -------------------------------------------------------------------------- }
{ Evento de criacao: configura parametros mas NAO conecta ao banco.           }
{ A conexao so acontece em EnsureConnected (primeira requisicao do usuario). }
{ -------------------------------------------------------------------------- }
procedure TDM.DataModuleCreate(Sender: TObject);
begin
  FConectado := False;
  ConfigurarConexao;
end;

{ -------------------------------------------------------------------------- }
{ Le o arquivo Config.ini para obter host, porta, usuario e senha do MySQL.  }
{ Usa TIniFile - classe padrao do Delphi para ler arquivos .ini             }
{ -------------------------------------------------------------------------- }
procedure TDM.ConfigurarConexao;
var
  Ini: TIniFile;
  IniPath: string;
begin
  { ParamStr(0) retorna o caminho completo do .exe }
  IniPath := ExtractFilePath(ParamStr(0)) + 'Config.ini';

  { Fail-fast: se nao encontrar o arquivo, lanca excecao imediatamente }
  if not FileExists(IniPath) then
    raise Exception.Create('Config.ini nao encontrado em: ' + IniPath);

  Ini := TIniFile.Create(IniPath);
  try
    { Configura os parametros do FireDAC para MySQL }
    FDConnection.Params.Clear;
    FDConnection.Params.DriverID  := 'MySQL';
    FDConnection.Params.Database  := Ini.ReadString('Database', 'Database', 'agenda_contatos');
    FDConnection.Params.UserName  := Ini.ReadString('Database', 'User', 'root');
    FDConnection.Params.Password  := Ini.ReadString('Database', 'Password', '');
    FDConnection.Params.Add('Server='   + Ini.ReadString('Database', 'Host', 'localhost'));
    FDConnection.Params.Add('Port='     + Ini.ReadString('Database', 'Port', '3306'));
    { utf8mb4 suporta emojis e caracteres especiais }
    FDConnection.Params.Add('CharacterSet=utf8mb4');
  finally
    { Garante que o TIniFile e liberado mesmo se ocorrer erro }
    Ini.Free;
  end;
end;

{ -------------------------------------------------------------------------- }
{ Lazy Connection: conecta ao MySQL apenas na primeira vez.                  }
{ Evita conexoes desnecessarias durante a inicializacao.                     }
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
{                                                                            }
{  GROUP_CONCAT: funcao MySQL que concatena valores de varias linhas          }
{  em uma string separada por virgula. Ex: "(11) 9999, (11) 3333"            }
{                                                                            }
{  LEFT JOIN: traz contatos mesmo sem telefone (resultado pode ser NULL).    }
{  DISTINCT: evita duplicacao quando o filtro encontra no JOIN.              }
{  Parametros (:pFiltro): protege contra SQL Injection.                      }
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

  Filtro := Trim(AFiltroNome);
  if Filtro <> '' then
    SQL := SQL + 'WHERE (c.NOME LIKE :pFiltro OR t.NUMERO LIKE :pFiltro) ';

  SQL := SQL + 'ORDER BY c.NOME';

  QryPesquisa.SQL.Text := SQL;

  { LIKE com % = busca parcial. Ex: '%silva%' encontra 'Maria Silva' }
  if Filtro <> '' then
    QryPesquisa.ParamByName('pFiltro').AsString := '%' + Filtro + '%';

  QryPesquisa.Open;
end;

{ -------------------------------------------------------------------------- }
{ Carrega um contato especifico para edicao.                                 }
{ Parametro :pID protege contra SQL Injection.                               }
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
{ Carrega telefones de um contato. Se AIDContato=0, retorna vazio.           }
{ ORDER BY ID garante ordem de insercao.                                     }
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
{ Prepara o QryContato para receber um novo registro.                        }
{ WHERE 1=0 retorna a estrutura das colunas sem dados.                       }
{ Append coloca a query em modo Insert (novo registro em memoria).           }
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
{ Grava o contato no banco usando CachedUpdates:                             }
{ 1. Post    = transfere dados do formulario para o cache em memoria         }
{ 2. ApplyUpdates(0) = envia o SQL real (INSERT ou UPDATE) para o MySQL      }
{    O parametro 0 significa: nao tolerar nenhum erro.                       }
{ -------------------------------------------------------------------------- }
function TDM.SalvarContato: Boolean;
begin
  Result := False;

  { Verifica se a query esta em modo de edicao ou insercao }
  if QryContato.State in [dsEdit, dsInsert] then
  begin
    if Trim(QryContato.FieldByName('NOME').AsString) = '' then
      Exit;

    QryContato.Post;
    QryContato.ApplyUpdates(0);
    Result := True;
  end;
end;

{ -------------------------------------------------------------------------- }
{ Exclui contato pelo ID. O ON DELETE CASCADE da FK remove                   }
{ automaticamente todos os telefones vinculados.                             }
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
{  LAST_INSERT_ID() e uma funcao MySQL que retorna o ultimo AUTO_INCREMENT.  }
{  Em caso de edicao (ID ja existe), retorna o ID corrente da query.        }
{ -------------------------------------------------------------------------- }
function TDM.SalvarContatoRetornarID: Integer;
begin
  Result := 0;
  if not SalvarContato then
    Exit;

  { LAST_INSERT_ID() retorna o ID gerado pelo ultimo INSERT }
  QryExec.SQL.Text := 'SELECT LAST_INSERT_ID() AS NOVO_ID';
  QryExec.Open;
  try
    if not QryExec.IsEmpty then
      Result := QryExec.FieldByName('NOVO_ID').AsInteger;
  finally
    QryExec.Close;
  end;

  { Fallback: se foi uma edicao, LAST_INSERT_ID pode retornar 0 }
  if (Result = 0) and QryContato.Active and not QryContato.IsEmpty then
    Result := QryContato.FieldByName('ID').AsInteger;
end;

{ -------------------------------------------------------------------------- }
{ Insere telefone diretamente via SQL (sem CachedUpdates).                   }
{ Usado porque o telefone e salvo imediatamente ao clicar "Adicionar".       }
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
{ Remove um telefone especifico pelo seu ID.                                 }
{ Usado pelo botao lixeira no grid de telefones.                             }
{ -------------------------------------------------------------------------- }
procedure TDM.ExcluirTelefonePorID(AIDTelefone: Integer);
begin
  EnsureConnected;
  QryExec.SQL.Text := 'DELETE FROM telefone WHERE ID = :pID';
  QryExec.ParamByName('pID').AsInteger := AIDTelefone;
  QryExec.ExecSQL;
end;

end.
