FROM haskell:latest

WORKDIR /app

COPY . .

# install dependencies properly
RUN cabal update
RUN cabal install --lib scotty aeson cassava text bytestring vector

# build project
RUN cabal build

EXPOSE 10000

CMD ["cabal", "run"]