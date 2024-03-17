# Use a base image with Haskell installed
FROM haskell:latest

# Set the working directory inside the container
WORKDIR /usr/src/app

# Copy the .cabal and .cabal.project files to the container
COPY . .

# Install dependencies based on the .cabal project file
RUN cabal update && cabal install --only-dependencies -j4

# Set the entry point to /bin/bash to access the container interactively
ENTRYPOINT ["/bin/bash"]
