@echo off
REM ============================================================
REM  executar.bat - Inicia o servidor Agenda de Contatos
REM
REM  Pre-requisito: executar configurar.bat uma vez antes.
REM ============================================================

echo.
echo ==========================================
echo   Agenda de Contatos - Iniciando...
echo ==========================================
echo.

REM Verificar se Config.ini existe (configurar.bat ja foi executado?)
if not exist "%~dp0bin\Config.ini" (
    echo [AVISO] Config.ini nao encontrado!
    echo         Executando configurar.bat para configurar o ambiente...
    echo.
    call "%~dp0configurar.bat"
    if not exist "%~dp0bin\Config.ini" (
        echo [ERRO] Configuracao falhou. Corrija os erros e tente novamente.
        pause
        exit /b 1
    )
)

REM Executar a aplicacao
if exist "%~dp0bin\AgendaContatos.exe" (
    echo Iniciando Agenda de Contatos...
    echo Acesse http://localhost:8080 no navegador apos clicar Iniciar.
    echo.
    start "" "%~dp0bin\AgendaContatos.exe"
) else (
    echo [ERRO] bin\AgendaContatos.exe nao encontrado!
    echo        Verifique se o arquivo existe na pasta bin.
    pause
)
