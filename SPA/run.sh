#!/bin/bash

# Install dependencies
echo "Installing dependencies..."
npm install

# Start the Angular server on the default port (4200)
echo "Starting the Angular server..."
ng serve --open

pause