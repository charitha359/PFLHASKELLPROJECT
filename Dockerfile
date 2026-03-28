FROM haskell:latest

WORKDIR /app

COPY . .

RUN cabal update

EXPOSE 10000

CMD ["./.stack-work/install/x86_64-linux/*/*/bin/PFLHASKELLPROJECT"]