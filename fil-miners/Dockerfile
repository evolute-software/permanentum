FROM haskell:8.10-buster

RUN cabal update

WORKDIR /app

COPY *.cabal .

RUN cabal build --only-dependencies -j4

COPY . .

RUN cabal build

RUN cp -v `cabal list-bin fil-miners:exes` /usr/bin

RUN type fil-miners

# Using INT has the benefit of being completely similar to
# terminal execution
STOPSIGNAL SIGINT
ENTRYPOINT [ "fil-miners" ]
