@echo off
setlocal EnableDelayedExpansion
REM ============================================================
REM  configurar.bat - Configuracao automatica do ambiente
REM
REM  Detecta MySQL (porta, caminho), pede credenciais,
REM  cria o banco de dados e gera o Config.ini automaticamente.
REM  Executar UMA vez no computador novo.
REM ============================================================

echo.
echo ==========================================
echo   Agenda de Contatos - Configuracao
echo ==========================================
echo.

REM -------------------------------------------------------
REM  1. LOCALIZAR O MYSQL (mysql.exe client)
REM -------------------------------------------------------
set "MYSQL_CLIENT="

REM Tenta caminhos comuns
for %%P in (
    "C:\Program Files\MySQL\MySQL Server 9.6\bin\mysql.exe"
    "C:\Program Files\MySQL\MySQL Server 9.0\bin\mysql.exe"
    "C:\Program Files\MySQL\MySQL Server 8.4\bin\mysql.exe"
    "C:\Program Files\MySQL\MySQL Server 8.0\bin\mysql.exe"
    "C:\Program Files (x86)\MySQL\MySQL Server 9.6\bin\mysql.exe"
    "C:\Program Files (x86)\MySQL\MySQL Server 8.4\bin\mysql.exe"
    "C:\Program Files (x86)\MySQL\MySQL Server 8.0\bin\mysql.exe"
) do (
    if exist %%P (
        set "MYSQL_CLIENT=%%~P"
        goto :client_found
    )
)

REM Tenta no PATH
where mysql.exe >nul 2>&1
if "%ERRORLEVEL%"=="0" (
    for /f "delims=" %%F in ('where mysql.exe') do (
        set "MYSQL_CLIENT=%%F"
        goto :client_found
    )
)

echo [ERRO] MySQL nao encontrado no computador!
echo.
echo Instale o MySQL Community Server:
echo   https://dev.mysql.com/downloads/mysql/
echo.
echo Apos instalar, execute este script novamente.
echo.
pause
exit /b 1

:client_found
echo [OK] MySQL encontrado em: %MYSQL_CLIENT%
echo.

REM -------------------------------------------------------
REM  2. DETECTAR PORTA DO MYSQL
REM -------------------------------------------------------
set "MYSQL_PORT=3306"

REM Tenta detectar via conexao na porta padrao 3306
"%MYSQL_CLIENT%" -u root --port=3306 -e "SELECT 1" >nul 2>&1
if "%ERRORLEVEL%"=="0" (
    set "MYSQL_PORT=3306"
    set "MYSQL_PASS="
    echo [OK] MySQL respondendo na porta 3306 (sem senha^)
    goto :port_found
)

REM Tenta com outras portas comuns
for %%T in (3307 3308 33060) do (
    "%MYSQL_CLIENT%" -u root --port=%%T -e "SELECT 1" >nul 2>&1
    if "!ERRORLEVEL!"=="0" (
        set "MYSQL_PORT=%%T"
        set "MYSQL_PASS="
        echo [OK] MySQL respondendo na porta %%T (sem senha^)
        goto :port_found
    )
)

REM Se nenhuma porta sem senha funcionou, pedir credenciais
echo MySQL requer autenticacao. Informe os dados de conexao:
echo.

set /p "MYSQL_USER=Usuario [root]: "
if "%MYSQL_USER%"=="" set "MYSQL_USER=root"

set /p "MYSQL_PASS=Senha: "

set /p "MYSQL_PORT_INPUT=Porta [3306]: "
if "%MYSQL_PORT_INPUT%"=="" set "MYSQL_PORT_INPUT=3306"
set "MYSQL_PORT=%MYSQL_PORT_INPUT%"

echo.
echo Testando conexao...

if "%MYSQL_PASS%"=="" (
    "%MYSQL_CLIENT%" -u %MYSQL_USER% --port=%MYSQL_PORT% -e "SELECT 1" >nul 2>&1
) else (
    "%MYSQL_CLIENT%" -u %MYSQL_USER% -p%MYSQL_PASS% --port=%MYSQL_PORT% -e "SELECT 1" >nul 2>&1
)

if "%ERRORLEVEL%" NEQ "0" (
    echo.
    echo [ERRO] Nao foi possivel conectar ao MySQL!
    echo        Verifique usuario, senha e porta.
    echo.
    pause
    exit /b 1
)

echo [OK] Conexao estabelecida com sucesso!
goto :credentials_set

:port_found
set "MYSQL_USER=root"

:credentials_set
echo.

REM -------------------------------------------------------
REM  3. CRIAR BANCO DE DADOS
REM -------------------------------------------------------
echo Criando banco de dados agenda_contatos...

if "%MYSQL_PASS%"=="" (
    "%MYSQL_CLIENT%" -u %MYSQL_USER% --port=%MYSQL_PORT% < "%~dp0database.sql" 2>&1
) else (
    "%MYSQL_CLIENT%" -u %MYSQL_USER% -p%MYSQL_PASS% --port=%MYSQL_PORT% < "%~dp0database.sql" 2>&1
)

if "%ERRORLEVEL%" NEQ "0" (
    echo.
    echo [AVISO] Pode ter ocorrido um erro ao criar o banco.
    echo         Se as tabelas ja existem, isso e normal.
) else (
    echo [OK] Banco de dados criado com sucesso!
)

echo.

REM -------------------------------------------------------
REM  4. GERAR CONFIG.INI
REM -------------------------------------------------------
echo Gerando Config.ini...

set "CONFIG_PATH=%~dp0bin\Config.ini"

(
echo [Database]
echo Host=localhost
echo Port=%MYSQL_PORT%
echo Database=agenda_contatos
echo User=%MYSQL_USER%
echo Password=%MYSQL_PASS%
echo [D2Bridge Server Config]
echo Server Port=8080
echo Server Name=D2Bridge Server
echo Server Description=D2Bridge Server
) > "%CONFIG_PATH%"

echo [OK] Config.ini salvo em: %CONFIG_PATH%
echo.

REM -------------------------------------------------------
REM  5. RESUMO
REM -------------------------------------------------------
echo ==========================================
echo   Configuracao concluida!
echo ==========================================
echo.
echo   MySQL:    localhost:%MYSQL_PORT%
echo   Usuario:  %MYSQL_USER%
echo   Banco:    agenda_contatos
echo   Config:   bin\Config.ini
echo.
echo   Para iniciar o programa, execute:
echo     executar.bat
echo.
pause
