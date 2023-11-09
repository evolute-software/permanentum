FROM ubuntu:22.04
# Unsuported node #FROM ubuntu:23.10

ARG BOOST_VERSION
ARG BOOST_GO_VERSION=1.21.3


# Deps
RUN apt-get update \
    && apt-get install -y mesa-opencl-icd \
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
                          ca-certificates \
                          gnupg \
    && rm -rf /var/lib/apt/lists/*

# Node16
# https://github.com/nodesource/distributions#installation-instructions
ENV NODE_MAJOR=16
RUN mkdir -p /etc/apt/keyrings \
    && curl -fsSL https://deb.nodesource.com/gpgkey/nodesource-repo.gpg.key | gpg --dearmor -o /etc/apt/keyrings/nodesource.gpg \
    && echo "deb [signed-by=/etc/apt/keyrings/nodesource.gpg] https://deb.nodesource.com/node_$NODE_MAJOR.x nodistro main" | tee /etc/apt/sources.list.d/nodesource.list \
    && apt-get update -y \
    && apt-get install -y nodejs \
    && rm -rf /var/lib/apt/lists/*

# Go
RUN curl -L -o go.tar.gz \
            https://go.dev/dl/go${BOOST_GO_VERSION}.linux-amd64.tar.gz \
         && rm -rf /usr/local/go \
         && tar -C /usr/local -xzf go.tar.gz

ENV PATH=$PATH:/usr/local/go/bin:/root/go/bin
RUN go version

# Rust
ENV CARGO_HOME=/usr/local/cargo
ENV RUSTUP_HOME=/usr/local/rustup
RUN curl -o /usr/local/bin/rustup \
        --proto '=https' \
        --tlsv1.2 \
        -sSf https://sh.rustup.rs \
    && chmod +x /usr/local/bin/rustup \
    && rustup -y

ENV PATH=$PATH:/usr/local/cargo/bin

ENV RUSTFLAGS="-C target-cpu=native -g"
ENV FFI_BUILD_FROM_SOURCE=1
ARG BOOST_BUILD_TARGET

# Boost
RUN git clone https://github.com/filecoin-project/boost \
    && cd boost \
    && git checkout $BOOST_VERSION \
    && make clean $BOOST_BUILD_TARGET \
    && make install

COPY get-offers.sh /usr/local/bin/get-offers.sh
COPY *.addr /root/

ARG FILECOIN_NETWORK
ENV FILECOIN_NETWORK=$FILECOIN_NETWORK


EXPOSE 8044
