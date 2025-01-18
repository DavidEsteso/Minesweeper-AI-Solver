FROM haskell:latest

WORKDIR /app

RUN cabal update

COPY . .

RUN cabal build
