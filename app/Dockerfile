FROM haskell:latest

WORKDIR /app

COPY . .

RUN cabal update
RUN cabal build

EXPOSE 10000

CMD ["cabal", "run"]