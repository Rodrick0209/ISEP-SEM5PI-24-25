@echo off

:: Restore NuGet packages
echo Restoring packages...
dotnet restore ../sem5pi_g55_24_25.sln
if %ERRORLEVEL% neq 0 (
    echo Restore failed, exiting...
    exit /b %ERRORLEVEL%
)

:: Build the project
echo Building MastersData...
dotnet build ../sem5pi_g55_24_25.sln
if %ERRORLEVEL% neq 0 (
    echo Build failed, exiting...
    exit /b %ERRORLEVEL%
)

:: Run the tests
echo Running tests...
dotnet test ../sem5pi_g55_24_25.sln
if %ERRORLEVEL% neq 0 (
    echo Tests failed, exiting...
    exit /b %ERRORLEVEL%
)

:: Run the project
echo Running MastersData...
dotnet run --project src
if %ERRORLEVEL% neq 0 (
    echo Failed to run the project, exiting...
    exit /b %ERRORLEVEL%
)