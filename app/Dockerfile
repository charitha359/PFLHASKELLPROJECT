FROM haskell:latest

WORKDIR /app

COPY . .

# initialize cabal
RUN cabal update

# configure project
RUN cabal configure

# build project
RUN cabal build

EXPOSE 10000

CMD ["cabal", "run"]