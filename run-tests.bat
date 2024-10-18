@echo off

:: Run tests
echo Running tests...
dotnet test
if %ERRORLEVEL% neq 0 (
    echo Tests failed, exiting...
    exit /b %ERRORLEVEL%
)

echo All tests passed.
pause