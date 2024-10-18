#!/bin/bash

# Run tests
echo "Running tests..."
dotnet test
if [ $? -ne 0 ]; then
    echo "Tests failed, exiting..."
    exit 1
fi

echo "All tests passed."
pause