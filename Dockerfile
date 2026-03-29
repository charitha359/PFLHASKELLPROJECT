FROM haskell:9.2

WORKDIR /app

COPY . .

# 🚨 REMOVE STACK COMPLETELY
RUN rm -rf /root/.stack

# 🚨 FORCE CABAL ONLY
RUN cabal update && cabal build

CMD ["cabal", "run", "PFLHASKELLPROJECT"]