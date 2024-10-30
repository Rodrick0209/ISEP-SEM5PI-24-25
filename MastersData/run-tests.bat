@echo off

:: Run the tests
echo Running tests...
dotnet test ../sem5pi_g55_24_25.sln
if %ERRORLEVEL% neq 0 (
    echo Tests failed, exiting...
    exit /b %ERRORLEVEL%
)
