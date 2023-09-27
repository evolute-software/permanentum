ARG UBUNTU_VERSION=20.04
FROM ubuntu:${UBUNTU_VERSION} as system_deps
ENV DEBIAN_FRONTEND=nonintercative
RUN mkdir -p /build
WORKDIR /build

# system_deps args
ARG IOHK_LIBSODIUM_GIT_REV=66f017f16633f2060db25e17c170c2afa0f2a8a1
ARG IOKH_LIBSECP251_GIT_REV=ac83be33d0956faf6b7f61a60ab524ef7d6a473a

# development dependencies
RUN apt-get update -y && apt-get install -y \
  curl \
  xz-utils \
  automake \
  build-essential \
  g++\
  git \
  jq \
  libicu-dev \
  libffi-dev \
  libgmp-dev \
  libncursesw5 \
  libpq-dev \
  libssl-dev \
  libsystemd-dev \
  libtinfo-dev \
  libtool \
  make \
  pkg-config \
  tmux \
  wget \
  zlib1g-dev libreadline-dev llvm libnuma-dev \
  && rm -rf /var/lib/apt/lists/*

# install secp2561k library with prefix '/'
RUN git clone https://github.com/bitcoin-core/secp256k1 &&\
  cd secp256k1 \
  && git fetch --all --tags &&\
  git checkout ${IOKH_LIBSECP251_GIT_REV} \
  && ./autogen.sh && \
  ./configure --prefix=/usr --enable-module-schnorrsig --enable-experimental && \
  make && \
  make install  && cd .. && rm -rf ./secp256k1

FROM system_deps as haskell

# haskell args
ARG CABAL_VERSION=3.6.2.0
ARG GHC_VERSION=8.10.7
ARG HLS_VERSION=1.7.0.0
ARG CARDANO_NODE_VERSION=1.35.7

# install libsodium from sources with prefix '/'
RUN git clone https://github.com/input-output-hk/libsodium.git &&\
  cd libsodium \
  && git fetch --all --tags &&\
  git checkout ${IOHK_LIBSODIUM_GIT_REV} \
  && ./autogen.sh && \
  ./configure --prefix=/usr && \
  make && \
  make install  && cd .. && rm -rf ./libsodium

##############################################################################
# Create the gero user
RUN adduser --disabled-password --gecos '' gero \
  && mkdir -m 0755 /nix /workspace \
  && chown -R gero:gero /build /nix /workspace

RUN cp -rp /root /home/gero/root \
  && chown -R gero /home/gero/root
RUN apt-get -y update && apt-get -y install  sudo \
  && echo -n "" > /etc/sudoers.d/gero \
  && echo "gero ALL=(ALL) NOPASSWD: /usr/bin/open-ipc" >> /etc/sudoers.d/gero \
  && echo "gero ALL=(ALL) NOPASSWD: /usr/bin/apt-get" >> /etc/sudoers.d/gero \
  && echo "gero ALL=(ALL) NOPASSWD: /usr/bin/cp" >> /etc/sudoers.d/gero \
  && echo "chmod 0777 /run/cardano/node.socket" > /usr/bin/open-ipc \
  && chmod +x /usr/bin/open-ipc

##############################################################################
# Become gero
USER gero
ENV USER gero
WORKDIR /home/gero

# install ghcup
ENV PATH=${PATH}:/home/gero/.ghcup/bin
RUN wget --secure-protocol=TLSv1_2 \
  https://downloads.haskell.org/~ghcup/$(arch)-linux-ghcup  \
  && chmod +x $(arch)-linux-ghcup \
  && mkdir -p ${HOME:-/root}/.ghcup/bin \
  && mv $(arch)-linux-ghcup ${HOME:-/root}/.ghcup/bin/ghcup

# install ghc, caball, and hls
RUN ghcup config set downloader Wget \
  && ghcup install ghc ${GHC_VERSION} \
  && ghcup install cabal ${CABAL_VERSION}
RUN ghcup install hls ${HLS_VERSION}
RUN ghcup set ghc ${GHC_VERSION}


# Update cabal
RUN cabal update

# Add cabal to PATH
RUN echo "export PATH=$PATH:/root/.cabal/bin" >> ~/.bashrc

WORKDIR /build

FROM haskell as cardano_node

RUN cd /build \
  && git clone https://github.com/input-output-hk/cardano-node.git \
  && cd cardano-node \
  && git fetch --all --recurse-submodules --tags \
  && git tag \
  && git checkout tags/$CARDANO_NODE_VERSION \
  && cabal update \
  && cabal build all \
  && sudo cp -p "`cabal list-bin cardano-node:exes`" "`cabal list-bin cardano-cli:exes`" /usr/bin/ \
  && cardano-node --version

# node socket
RUN echo "export CARDANO_NODE_SOCKET_PATH=/root/.cardano/preview/node.socket" >> ~/.bashrc
RUN echo "export NETWORK=preview" >> ~/.bashrc

FROM cardano_node as kuber

# TODO: Install kuber-playground
# install kuber

RUN cd /build \
  && git clone https://github.com/dQuadrant/kuber.git \
  && cd kuber \
  && git checkout 2.3.2 \
  && cabal build exe:kuber \
  && sudo cp `find . -name kuber -type f` /usr/bin/

FROM kuber as stylish_haskell

RUN sudo apt-get update -y \
  && sudo apt-get install -y stylish-haskell \
  && type stylish-haskell \
  && stylish-haskell --version

FROM kuber as nodejs

ENV NODE_VERSION=16.13.0
RUN curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.0/install.sh | bash
ENV NVM_DIR=/home/gero/.nvm
RUN . "$NVM_DIR/nvm.sh" && nvm install ${NODE_VERSION}
RUN . "$NVM_DIR/nvm.sh" && nvm use v${NODE_VERSION}
RUN . "$NVM_DIR/nvm.sh" && nvm alias default v${NODE_VERSION}
ENV PATH="${NVM_DIR}/versions/node/v${NODE_VERSION}/bin/:${PATH}"
RUN node --version
RUN npm --version

FROM nodejs as plutus

ARG PPP_VERSION=fourth-iteration


RUN git clone -b $PPP_VERSION \
      https://github.com/input-output-hk/plutus-pioneer-program \
  && cd plutus-pioneer-program/code \
  && cabal update \
  && cabal build all

RUN echo "sudo  /usr/bin/open-ipc" >> /home/gero/.bashrc

# Install nix, allow it to run flake commands, add IOG binary cache
RUN cd /home/gero/ \
  && curl -L https://nixos.org/nix/install | sh \
  && mkdir -p .config/nix \
  && echo "experimental-features = nix-command flakes" >> .config/nix/nix.conf \
  && echo "substituters = https://hydra.iohk.io https://iohk.cachix.org https://cache.nixos.org/" >> .config/nix/nix.conf \
  && echo "trusted-public-keys   = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=" >> .config/nix/nix.conf \
  && echo . /home/gero/.nix-profile/etc/profile.d/nix.sh >> .bashrc \
  && . /home/gero/.nix-profile/etc/profile.d/nix.sh \
  && type nix \
  && nix --version \
  && nix-channel --update

# Clone and bouild a deployable plutus version
#ARG PLUTUS_VERSION=1.8.0.0
#
# https://github.com/input-output-hk/cardano-haskell-packages/blob/b3105675c9252d90ba8c7d4fcdceed08a1841e89/_sources/plutus-tx/1.0.0.0/meta.toml
# PPP v4 and cardano-node 1.35.3 (last supported right now) use:
ARG PLUTUS_VERSION=a56c96598b4b25c9e28215214d25189331087244
RUN cd /build \
  && git clone --recurse-submodules \
      https://github.com/input-output-hk/plutus.git plutus \
  && cd plutus \
  && git checkout ${PLUTUS_VERSION} \
  && git clean -f \
  && git reset --hard
 # && . /home/gero/.nix-profile/etc/profile.d/nix.sh \
 # && ls -la \
 # && nix build -f default.nix

# 1.8 and later:
#  && nix develop --accept-flake-config --command cabal update \
#  && nix develop --accept-flake-config --command cabal build all

RUN cd /build/plutus \
  && . /home/gero/.nix-profile/etc/profile.d/nix.sh \
  && nix build -f default.nix  plutus.plutus-haddock-combined
  #&& mv result/share/doc /build/plutus-haddock
# 1.8 and later:
#  && nix develop --accept-flake-config --command build-docs \
#  && mv doc/_build /build/plutus-docs

RUN cd /build/plutus \
  && . /home/gero/.nix-profile/etc/profile.d/nix.sh \
  && nix build -f default.nix docs \
  && mkdir /build/plutus-docs \
  && cp -r result/* /build/plutus-docs/

RUN cd /build \
  && git clone -b v0.7.3.0 https://github.com/hvr/cabal-plan \
  && cd cabal-plan \
  && cabal build exe:cabal-plan \
  && sudo cp `find dist-newstyle/ -type f -name cabal-plan` /usr/bin

RUN echo "source <(cardano-cli --bash-completion-script cardano-cli)" >> ~/.bashrc

