FROM haskell:latest

COPY *.cabal ./
RUN cabal install --dependencies-only -j4 --enable-tests

COPY . /app
WORKDIR /app