@echo off
echo ==========================================
echo   Iniciando MySQL Server...
echo ==========================================
echo.

REM Verifica se MySQL ja esta rodando
tasklist /FI "IMAGENAME eq mysqld.exe" 2>NUL | find /I /N "mysqld.exe">NUL
if "%ERRORLEVEL%"=="0" (
    echo [OK] MySQL ja esta rodando na maquina.
    echo      Nao e necessario iniciar novamente.
    echo.
    pause
    exit /b 0
)

REM Detecta automaticamente onde o MySQL esta instalado
set "MYSQL_BIN="

REM Tenta caminhos comuns do MySQL
for %%P in (
    "C:\Program Files\MySQL\MySQL Server 9.6\bin\mysqld.exe"
    "C:\Program Files\MySQL\MySQL Server 8.4\bin\mysqld.exe"
    "C:\Program Files\MySQL\MySQL Server 8.0\bin\mysqld.exe"
    "C:\Program Files\MySQL\MySQL Server 9.0\bin\mysqld.exe"
    "C:\Program Files (x86)\MySQL\MySQL Server 8.4\bin\mysqld.exe"
    "C:\Program Files (x86)\MySQL\MySQL Server 8.0\bin\mysqld.exe"
) do (
    if exist %%P (
        set "MYSQL_BIN=%%~P"
        goto :found
    )
)

REM Tenta localizar via registro do Windows
for /f "tokens=2*" %%A in ('reg query "HKLM\SOFTWARE\MySQL AB" /s /v Location 2^>nul ^| findstr /i "Location"') do (
    if exist "%%B\bin\mysqld.exe" (
        set "MYSQL_BIN=%%B\bin\mysqld.exe"
        goto :found
    )
)

REM Tenta localizar no PATH do sistema
where mysqld.exe >nul 2>&1
if "%ERRORLEVEL%"=="0" (
    for /f "delims=" %%F in ('where mysqld.exe') do (
        set "MYSQL_BIN=%%F"
        goto :found
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

:found
echo [OK] MySQL encontrado em: %MYSQL_BIN%

REM Extrai o basedir (pasta pai de bin\)
for %%D in ("%MYSQL_BIN%") do set "MYSQL_BASE=%%~dpD"
for %%D in ("%MYSQL_BASE%..") do set "MYSQL_BASE=%%~fD"

REM Configura pasta de dados (na mesma pasta do projeto)
set "DATA_DIR=%~dp0mysql_data"

REM Se a pasta de dados nao existe, inicializa o banco
if not exist "%DATA_DIR%" (
    echo.
    echo Inicializando banco de dados MySQL pela primeira vez...
    echo Isso pode levar alguns segundos...
    "%MYSQL_BIN%" --initialize-insecure --basedir="%MYSQL_BASE%" --datadir="%DATA_DIR%"
    echo [OK] Banco inicializado.
)

echo Iniciando MySQL na porta 3306...
echo.
"%MYSQL_BIN%" --basedir="%MYSQL_BASE%" --datadir="%DATA_DIR%" --port=3306 --console
pause