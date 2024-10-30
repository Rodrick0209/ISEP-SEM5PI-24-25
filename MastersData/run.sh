#!/bin/bash

# Restore NuGet packages
echo "Restoring packages..."
dotnet restore
if [ $? -ne 0 ]; then
    echo "Restore failed, exiting..."
    exit 1
fi

# Build the project
echo "Building MastersData..."
dotnet build
if [ $? -ne 0 ]; then
    echo "Build failed, exiting..."
    exit 1
fi

# Run the tests
echo "Running tests..."
dotnet test ../sem5pi_g55_24_25.sln
if [ $? -ne 0 ]; then
    echo "Tests failed, exiting..."
    exit 1
fi

# Run the project
echo "Running MastersData.."
dotnet run --project MastersData/src
if [ $? -ne 0 ]; then
    echo "Failed to run the project, exiting..."
    exit 1
fi