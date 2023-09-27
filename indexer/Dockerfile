FROM ghcr.io/txpipe/oura:v1.8.1

RUN apt update -y \
  && apt install -y gettext \
  && rm -rf /var/lib/apt/lists/* \
  && rm -rf rm -rf /var/cache/apt/archives

COPY daemon.toml /daemon.toml.tpl

COPY entrypoint.sh /entrypoint.sh
RUN chmod +x /entrypoint.sh

ENTRYPOINT [ "/entrypoint.sh" ]
CMD \--help

EXPOSE 9186
