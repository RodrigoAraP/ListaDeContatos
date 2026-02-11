@echo off
REM ============================================================
REM  executar.bat - Inicia MySQL + Executa o servidor
REM ============================================================

echo.
echo ==========================================
echo   Agenda de Contatos - Iniciando...
echo ==========================================
echo.

REM 1. Verificar se MySQL ja esta rodando
tasklist /FI "IMAGENAME eq mysqld.exe" 2>NUL | find /I /N "mysqld.exe">NUL
if "%ERRORLEVEL%"=="0" (
    echo [OK] MySQL ja esta rodando.
    goto :start_app
)

REM Detecta automaticamente onde o MySQL esta instalado
set "MYSQL_BIN="

for %%P in (
    "C:\Program Files\MySQL\MySQL Server 8.4\bin\mysqld.exe"
    "C:\Program Files\MySQL\MySQL Server 8.0\bin\mysqld.exe"
    "C:\Program Files\MySQL\MySQL Server 9.0\bin\mysqld.exe"
    "C:\Program Files (x86)\MySQL\MySQL Server 8.4\bin\mysqld.exe"
    "C:\Program Files (x86)\MySQL\MySQL Server 8.0\bin\mysqld.exe"
) do (
    if exist %%P (
        set "MYSQL_BIN=%%~P"
        goto :mysql_found
    )
)

for /f "tokens=2*" %%A in ('reg query "HKLM\SOFTWARE\MySQL AB" /s /v Location 2^>nul ^| findstr /i "Location"') do (
    if exist "%%B\bin\mysqld.exe" (
        set "MYSQL_BIN=%%B\bin\mysqld.exe"
        goto :mysql_found
    )
)

where mysqld.exe >nul 2>&1
if "%ERRORLEVEL%"=="0" (
    for /f "delims=" %%F in ('where mysqld.exe') do (
        set "MYSQL_BIN=%%F"
        goto :mysql_found
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

:mysql_found
echo [OK] MySQL encontrado em: %MYSQL_BIN%

REM Extrai o basedir
for %%D in ("%MYSQL_BIN%") do set "MYSQL_BASE=%%~dpD"
for %%D in ("%MYSQL_BASE%..") do set "MYSQL_BASE=%%~fD"

REM Pasta de dados relativa ao projeto
set "DATA_DIR=%~dp0mysql_data"

REM Se a pasta de dados nao existe, inicializa
if not exist "%DATA_DIR%" (
    echo Inicializando banco de dados MySQL pela primeira vez...
    "%MYSQL_BIN%" --initialize-insecure --basedir="%MYSQL_BASE%" --datadir="%DATA_DIR%"
    echo [OK] Banco inicializado.
)

echo Iniciando MySQL...
start "MySQL Server" /MIN "%MYSQL_BIN%" --basedir="%MYSQL_BASE%" --datadir="%DATA_DIR%" --port=3306 --console
echo Aguardando MySQL iniciar...
timeout /t 3 /nobreak >nul
echo [OK] MySQL iniciado.

:start_app
echo.

REM 2. Executar a aplicacao
if exist "%~dp0bin\AgendaContatos.exe" (
    echo Iniciando Agenda de Contatos...
    echo Acesse http://localhost:8080 no navegador apos clicar Iniciar.
    echo.
    start "" "%~dp0bin\AgendaContatos.exe"
) else (
    echo ERRO: bin\AgendaContatos.exe nao encontrado!
    echo Execute compilar.bat primeiro.
    pause
)
