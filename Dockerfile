FROM haskell:latest

WORKDIR /app

COPY . .

RUN cabal update

EXPOSE 10000

CMD ["cabal", "run"]