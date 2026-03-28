FROM haskell:latest

WORKDIR /app

COPY . .

# install stack
RUN curl -sSL https://get.haskellstack.org/ | sh

# build project using stack
RUN stack setup
RUN stack build

EXPOSE 10000

CMD ["stack", "run"]