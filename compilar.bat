@echo off
REM ============================================================
REM  compilar.bat - Compila o projeto Agenda de Contatos
REM  Uso: compilar.bat
REM  Requer: Delphi instalado (rsvars.bat no PATH do RAD Studio)
REM ============================================================

echo.
echo ==========================================
echo   Compilando Agenda de Contatos
echo ==========================================
echo.

REM Detectar RAD Studio (tenta versoes comuns)
set "RSVARS="

REM Delphi 12
if exist "C:\Program Files (x86)\Embarcadero\Studio\23.0\bin\rsvars.bat" (
    set "RSVARS=C:\Program Files (x86)\Embarcadero\Studio\23.0\bin\rsvars.bat"
    goto :found
)

REM Delphi 11
if exist "C:\Program Files (x86)\Embarcadero\Studio\22.0\bin\rsvars.bat" (
    set "RSVARS=C:\Program Files (x86)\Embarcadero\Studio\22.0\bin\rsvars.bat"
    goto :found
)

REM Delphi 10.4
if exist "C:\Program Files (x86)\Embarcadero\Studio\21.0\bin\rsvars.bat" (
    set "RSVARS=C:\Program Files (x86)\Embarcadero\Studio\21.0\bin\rsvars.bat"
    goto :found
)

REM Community Edition
if exist "C:\Program Files (x86)\Embarcadero\Studio\23.0\bin\rsvars.bat" (
    set "RSVARS=C:\Program Files (x86)\Embarcadero\Studio\23.0\bin\rsvars.bat"
    goto :found
)

echo ERRO: RAD Studio nao encontrado!
echo Instale o Delphi Community Edition em:
echo   https://www.embarcadero.com/products/delphi/starter/free-download
echo.
pause
exit /b 1

:found
echo RAD Studio encontrado: %RSVARS%
echo.

REM Carregar variaveis de ambiente do Delphi
call "%RSVARS%"

REM Criar pasta de saida
if not exist bin mkdir bin

REM Compilar com MSBuild
echo Compilando...
msbuild AgendaContatos.dproj /t:Build /p:Config=Debug /p:Platform=Win32 /v:minimal

if %ERRORLEVEL% NEQ 0 (
    echo.
    echo ERRO na compilacao! Verifique os erros acima.
    pause
    exit /b 1
)

echo.
echo ==========================================
echo   Compilacao concluida com sucesso!
echo   Executavel: bin\AgendaContatos.exe
echo ==========================================
echo.

REM Copiar libmysql.dll e Config.ini para bin se nao existirem
if exist libmysql.dll (
    if not exist bin\libmysql.dll copy libmysql.dll bin\ >nul
)
if exist Config.ini (
    if not exist bin\Config.ini copy Config.ini bin\ >nul
)

pause
