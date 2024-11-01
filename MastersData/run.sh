#!/bin/bash

# Caminho do arquivo de solução
SOLUTION_PATH="../sem5pi_g55_24_25.sln"

# Restore NuGet packages
echo "Restoring packages..."
dotnet restore $SOLUTION_PATH
if [ $? -ne 0 ]; then
    echo "Restore failed, exiting..."
    exit 1
fi

# Build the project
echo "Building the project..."
dotnet build $SOLUTION_PATH --no-restore
if [ $? -ne 0 ]; then
    echo "Build failed, exiting..."
    exit 1
fi


# Run the project
echo "Starting the application..."
dotnet run --project src/DDDNetCore.csproj
if [ $? -ne 0 ]; then
    echo "Failed to run the project, exiting..."
    exit 1
fi