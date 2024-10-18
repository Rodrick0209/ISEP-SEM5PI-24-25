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

# Run the project
echo "Running MastersData.."
dotnet run --project MastersData
if [ $? -ne 0 ]; then
    echo "Failed to run the project, exiting..."
    exit 1
fi