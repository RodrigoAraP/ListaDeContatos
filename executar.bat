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
) else (
    echo Iniciando MySQL...
    start "MySQL Server" /MIN "C:\Program Files\MySQL\MySQL Server 8.4\bin\mysqld.exe" --basedir="C:\Program Files\MySQL\MySQL Server 8.4" --datadir="C:\Development\mysql_data" --port=3306 --console
    echo Aguardando MySQL iniciar...
    timeout /t 3 /nobreak >nul
    echo [OK] MySQL iniciado.
)

echo.

REM 2. Executar a aplicacao
if exist bin\AgendaContatos.exe (
    echo Iniciando Agenda de Contatos...
    echo Acesse http://localhost:8080 no navegador apos clicar Iniciar.
    echo.
    start bin\AgendaContatos.exe
) else (
    echo ERRO: bin\AgendaContatos.exe nao encontrado!
    echo Execute compilar.bat primeiro.
    pause
)
