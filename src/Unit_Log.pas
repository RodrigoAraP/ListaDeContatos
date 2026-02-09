{ ============================================================================
  Unit_Log.pas  -  Sistema de Log de Alteracoes

  Grava todas as operacoes (criacao, edicao, exclusao de contatos e
  telefones) no arquivo log_alteracoes.txt com timestamp.
  ============================================================================ }
unit Unit_Log;

interface

uses
  System.SysUtils, System.Classes;

type
  TTipoLog = (tlCriacao, tlEdicao, tlExclusao, tlTelefoneAdd, tlTelefoneRemove);

procedure GravarLog(ATipo: TTipoLog; const ADescricao: string);

{ Atalhos para facilitar chamadas }
procedure GravarLogExclusao(const AID: Integer; const ANome: string;
  const AIdade: Integer; const ATelefones: string);
procedure GravarLogCriacao(const AID: Integer; const ANome: string;
  const AIdade: Integer);
procedure GravarLogEdicao(const AID: Integer; const ANome: string;
  const AIdade: Integer);

implementation

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

procedure GravarLog(ATipo: TTipoLog; const ADescricao: string);
var
  LogPath: string;
  Writer: TStreamWriter;
begin
  LogPath := ExtractFilePath(ParamStr(0)) + 'log_alteracoes.txt';
  try
    Writer := TStreamWriter.Create(LogPath, True, TEncoding.UTF8);
    try
      Writer.WriteLine(Format('[%s] [%s] %s',
        [FormatDateTime('yyyy-mm-dd hh:nn:ss', Now),
         TipoLogStr(ATipo),
         ADescricao]));
    finally
      Writer.Free;
    end;
  except
    { Falha ao gravar log nao deve interromper a operacao do usuario }
  end;
end;

procedure GravarLogExclusao(const AID: Integer; const ANome: string;
  const AIdade: Integer; const ATelefones: string);
begin
  GravarLog(tlExclusao,
    Format('Contato excluido - ID: %d | Nome: %s | Idade: %d | Telefones: %s',
      [AID, ANome, AIdade, ATelefones]));
end;

procedure GravarLogCriacao(const AID: Integer; const ANome: string;
  const AIdade: Integer);
begin
  GravarLog(tlCriacao,
    Format('Contato criado - ID: %d | Nome: %s | Idade: %d',
      [AID, ANome, AIdade]));
end;

procedure GravarLogEdicao(const AID: Integer; const ANome: string;
  const AIdade: Integer);
begin
  GravarLog(tlEdicao,
    Format('Contato editado - ID: %d | Nome: %s | Idade: %d',
      [AID, ANome, AIdade]));
end;

end.
