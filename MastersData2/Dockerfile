# syntax=docker/dockerfile:1

ARG NODE_VERSION=20.5.0

FROM node:${NODE_VERSION}-alpine

# Install Python and other necessary dependencies for building native modules.
RUN apk update && apk add --no-cache \
    python3 \
    py3-pip \
    build-base \
    bash

# Use production node environment by default.
ENV NODE_ENV production

WORKDIR /usr/src/app

# Copy package.json and package-lock.json
COPY package*.json ./

# Install dependencies
RUN npm install

# Copy the rest of the source files into the image.
COPY . .

# Expose the port that the application listens on.
EXPOSE 2226

# Run the application.
CMD npm start