# Use a base image with Haskell installed
FROM haskell:latest

# Set the working directory inside the container
WORKDIR /usr/src/app

# Copy the cabal files first to leverage Docker cache
COPY . . 

# Install dependencies
RUN cabal update
RUN cabal install --only-dependencies -j4

# Expose necessary ports (if your application listens on a port)
EXPOSE 8080

ENTRYPOINT ["/bin/bash"]
