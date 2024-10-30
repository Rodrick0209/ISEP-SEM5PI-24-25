@echo off

:: Install dependencies
echo Installing dependencies...
call npm install

:: Start the Angular server on the default port (4200)
echo Starting the Angular server...
call ng serve --open

pause