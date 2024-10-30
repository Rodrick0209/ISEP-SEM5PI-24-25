#!/bin/bash

# Run the tests
echo "Running tests..."
dotnet test ../sem5pi_g55_24_25.sln
if [ $? -ne 0 ]; then
    echo "Tests failed, exiting..."
    exit 1
fi
