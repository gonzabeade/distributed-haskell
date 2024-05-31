# 

## Overview
`distributed-haskell` is a Distributed State Machine in Haskell. This project provides a simple implementation of a leader-based consensus algorithm, enabling distributed coordination among multiple nodes.

## Features
- Distributed logging
- RESTful API for command execution
- Dockerized setup for easy deployment

## Prerequisites 
- Docker (optional)
- Docker Compose (optional)

## Installation
1. Clone the repository:
    ```sh
    git clone https://github.com/gonzabeade/distributed-haskell.git
    cd distributed-haskell
    ```

2. Build and run the Docker containers:
    ```sh
    docker compose up
    ```

3. Access the shell of a worker container:
    ```sh
    sudo docker-compose exec haskell-worker-1 /bin/bash
    ```

## Project Structure
- **Master.hs**: Main entry point for the master node.
- **Worker.hs**: Main entry point for worker nodes.
- **FileSystemMonad.hs**: Defines the file system operations.
- **Log.hs**: Contains the log data structure and related functions.
- **Log2Monad.hs**: Translates logs into monadic actions.

## Usage
To interact with the running containers, you can use the provided RESTful API.

### API Endpoints

Just in case you want to communicate with the nodes via API rather than via the provided shell in `Worker.hs`, here are the endpoints: 

- **POST /commands**: Execute a command.
  - Parameters:
    - `nodeId`: ID of the node
    - `cmd`: Command to execute (touch, mkdir, rm, write)
    - `path`: Path of the file/directory
    - `content`: Content to write (optional)
    - `lastLogId`: ID of the last log entry (optional)
- **GET /log**: Retrieve the log.

### Sample Commands
1. Create a new file:
    ```sh
    curl -X POST "http://localhost:8080/commands?cmd=touch&path=/tmp/newfile.txt&nodeId=node1"
    ```

2. Create a new directory:
    ```sh
    curl -X POST "http://localhost:8080/commands?cmd=mkdir&path=/tmp/newdir&nodeId=node1"
    ```

3. Write to a file:
    ```sh
    curl -X POST "http://localhost:8080/commands?cmd=write&path=/tmp/newfile.txt&content=HelloWorld&nodeId=node1"
    ```

4. Get the current log:
    ```sh
    curl -X GET "http://localhost:8080/log"
    ```

## Development
1. Install dependencies:
    ```sh
    cabal update
    cabal install
    ```

2. Build the project:
    ```sh
    cabal build
    ```

3. Run the master node:
    ```sh
    cabal run master
    ```

3. Run worker nodes:
    ```sh
    cabal run worker
    ```