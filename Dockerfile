# Use a base image with Haskell installed
FROM haskell:latest

# Set the working directory inside the container
WORKDIR /usr/src/app

# Copy only the .cabal file first to leverage Docker cache
COPY . .

# Install dependencies
RUN cabal update
RUN cabal install

# Set the default command to run when the container starts
CMD ["cabal", "run"]
