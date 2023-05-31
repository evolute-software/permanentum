FROM golang:1.19.7-buster

# Based on:
# https://lotus.filecoin.io/lotus/install/linux/

# The version of lotus you want
ARG VERSION

# Please only pass this one as a build-arg, not an env!
ARG NET
ENV NET $NET


ENV RUSTFLAGS "-C target-cpu=native -g"
ENV FFI_BUILD_FROM_SOURCE 1
ENV CGO_CFLAGS_ALLOW "-D__BLST_PORTABLE__"
ENV CGO_CFLAGS "-D__BLST_PORTABLE__"

# Build Req
RUN apt update && apt install -y \
  mesa-opencl-icd \
  ocl-icd-opencl-dev \
  gcc \
  git \
  bzr \
  jq \
  pkg-config \
  curl \
  clang \
  build-essential \
  hwloc \
  libhwloc-dev \
  wget \
  aria2 \
  && du -hs /var/lib/apt/lists /var/cache/apt/archives \
  && rm -rf /var/lib/apt/lists/* /var/cache/apt/archives/*
# TODO: if we only need hwloc in the runtime we can get rid of a lot of 
#       image size by adding a runtime layer

# Install rustup
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs -o /opt/rustup-installer.sh
RUN chmod +x /opt/rustup-installer.sh
RUN sh /opt/rustup-installer.sh -vy
RUN cat $HOME/.cargo/env
RUN mkdir -p /usr/local/rust && mv $HOME/.cargo/bin /usr/local/rust/bin
ENV PATH=/usr/local/rust/bin:$PATH
RUN echo $PATH
RUN command -v rustup

RUN git clone https://github.com/filecoin-project/lotus.git /opt/filecoin/lotus
WORKDIR /opt/filecoin/lotus
RUN git checkout $VERSION
RUN git show HEAD
RUN bash -c 'echo Building $NET; [ "$NET" = "mainnet" ] && make clean all || make clean $NET'

RUN make install
RUN ls -la /usr/local/bin

# Add user
RUN useradd -ms /bin/bash  lotus

COPY entrypoint.sh /
RUN chmod +x /entrypoint.sh

USER lotus
WORKDIR /lotus
RUN echo $PATH
STOPSIGNAL TERM
ENTRYPOINT [ "/entrypoint.sh" ]



