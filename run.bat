@echo off

:: Restore NuGet packages
echo Restoring packages...
dotnet restore
if %ERRORLEVEL% neq 0 (
    echo Restore failed, exiting...
    exit /b %ERRORLEVEL%
)

:: Build the project
echo Building MastersData...
dotnet build
if %ERRORLEVEL% neq 0 (
    echo Build failed, exiting...
    exit /b %ERRORLEVEL%
)

:: Run the project
echo Running MastersData...
dotnet run --project MastersData
if %ERRORLEVEL% neq 0 (
    echo Failed to run the project, exiting...
    exit /b %ERRORLEVEL%
)