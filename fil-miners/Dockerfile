FROM haskell:9.2.8-buster

RUN cabal update

WORKDIR /app

COPY *.cabal .

RUN cabal build --only-dependencies -j4

COPY . .

RUN cabal build

RUN cp -v `cabal list-bin fil-miners-exe` /usr/bin/fil-miners

RUN type fil-miners

# Using INT has the benefit of being completely similar to
# terminal execution
STOPSIGNAL SIGINT
ENTRYPOINT [ "fil-miners" ]
