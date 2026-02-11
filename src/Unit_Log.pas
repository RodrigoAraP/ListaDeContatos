{ ============================================================================
  Unit_Log.pas  -  Sistema de Log de Alteracoes

  Grava todas as operacoes (criacao, edicao, exclusao de contatos e
  telefones) no arquivo log_alteracoes.txt com timestamp.

  Conceitos-chave:
  - TTipoLog: enum que categoriza cada operacao (CRIACAO, EDICAO etc.)
  - TStreamWriter: classe .NET-like para escrever texto em arquivo
  - try/except: garante que falha no log NAO interrompe o usuario
  - Funcoes auxiliares (GravarLogExclusao, etc.) simplificam chamadas
  - Formato: [yyyy-mm-dd hh:nn:ss] [TIPO] Descricao
  ============================================================================ }
unit Unit_Log;

interface

uses
  System.SysUtils, System.Classes;

type
  { Enum que define os tipos de operacao para categorizar no log.
    Cada valor gera um texto diferente no arquivo (CRIACAO, EDICAO etc.) }
  TTipoLog = (tlCriacao, tlEdicao, tlExclusao, tlTelefoneAdd, tlTelefoneRemove);

{ Funcao principal de gravacao de log - todas as outras chamam esta }
procedure GravarLog(ATipo: TTipoLog; const ADescricao: string);

{ Atalhos tipados: formatam a mensagem automaticamente e chamam GravarLog }
procedure GravarLogExclusao(const AID: Integer; const ANome: string;
  const AIdade: Integer; const ATelefones: string);
procedure GravarLogCriacao(const AID: Integer; const ANome: string;
  const AIdade: Integer);
procedure GravarLogEdicao(const AID: Integer; const ANome: string;
  const AIdade: Integer);

implementation

{ Converte o enum TTipoLog para texto legivel no arquivo de log.
  Usado internamente por GravarLog para montar a linha formatada. }
function TipoLogStr(ATipo: TTipoLog): string;
begin
  case ATipo of
    tlCriacao:         Result := 'CRIACAO';
    tlEdicao:          Result := 'EDICAO';
    tlExclusao:        Result := 'EXCLUSAO';
    tlTelefoneAdd:     Result := 'TEL_ADD';
    tlTelefoneRemove:  Result := 'TEL_REMOVE';
  else
    Result := 'OUTRO';
  end;
end;

{ -------------------------------------------------------------------------- }
{ GravarLog: grava uma linha no arquivo log_alteracoes.txt.                  }
{                                                                            }
{ Formato da linha:                                                          }
{   [2025-01-15 14:30:00] [CRIACAO] Contato criado - ID: 1 | Nome: Joao...  }
{                                                                            }
{ TStreamWriter com True no 2o parametro = modo APPEND (adiciona ao final).  }
{ TEncoding.UTF8 = garante que acentos e caracteres especiais ficam corretos.}
{ try/except vazio: se falhar ao gravar (disco cheio, permissao etc.),       }
{ a operacao do usuario NAO e interrompida.                                  }
{ -------------------------------------------------------------------------- }
procedure GravarLog(ATipo: TTipoLog; const ADescricao: string);
var
  LogPath: string;
  Writer: TStreamWriter;
begin
  { ParamStr(0) = caminho do .exe, ExtractFilePath pega so a pasta }
  LogPath := ExtractFilePath(ParamStr(0)) + 'log_alteracoes.txt';
  try
    { TStreamWriter: classe do Delphi para escrita sequencial em arquivo.
      Parametros: caminho, append (True = nao sobrescreve), encoding }
    Writer := TStreamWriter.Create(LogPath, True, TEncoding.UTF8);
    try
      { Format monta a string com placeholders %s.
        FormatDateTime converte TDateTime para texto legivel. }
      Writer.WriteLine(Format('[%s] [%s] %s',
        [FormatDateTime('yyyy-mm-dd hh:nn:ss', Now),
         TipoLogStr(ATipo),
         ADescricao]));
    finally
      Writer.Free; { Garante que o arquivo e fechado mesmo com erro }
    end;
  except
    { Silencia erros de I/O - log e importante mas nao critico.
      Se falhasse e nao fosse tratado, o usuario perderia a operacao. }
  end;
end;

{ -------------------------------------------------------------------------- }
{ Funcoes auxiliares: formatam a descricao e delegam para GravarLog.          }
{ Evitam que o chamador precise montar o Format manualmente.                 }
{ -------------------------------------------------------------------------- }

{ Log de exclusao: registra TODOS os dados do contato antes de apagar,
  incluindo telefones (pois serao perdidos pelo CASCADE). }
procedure GravarLogExclusao(const AID: Integer; const ANome: string;
  const AIdade: Integer; const ATelefones: string);
begin
  GravarLog(tlExclusao,
    Format('Contato excluido - ID: %d | Nome: %s | Idade: %d | Telefones: %s',
      [AID, ANome, AIdade, ATelefones]));
end;

{ Log de criacao: registra ID gerado, nome e idade do novo contato }
procedure GravarLogCriacao(const AID: Integer; const ANome: string;
  const AIdade: Integer);
begin
  GravarLog(tlCriacao,
    Format('Contato criado - ID: %d | Nome: %s | Idade: %d',
      [AID, ANome, AIdade]));
end;

{ Log de edicao: registra os dados ATUALIZADOS do contato }
procedure GravarLogEdicao(const AID: Integer; const ANome: string;
  const AIdade: Integer);
begin
  GravarLog(tlEdicao,
    Format('Contato editado - ID: %d | Nome: %s | Idade: %d',
      [AID, ANome, AIdade]));
end;

end.
