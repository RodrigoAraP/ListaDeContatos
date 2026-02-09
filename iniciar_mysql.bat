@echo off
echo Iniciando MySQL Server 8.4...
echo.
"C:\Program Files\MySQL\MySQL Server 8.4\bin\mysqld.exe" --basedir="C:\Program Files\MySQL\MySQL Server 8.4" --datadir="C:\Development\mysql_data" --port=3306 --console
pause