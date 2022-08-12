# Building wrk-websocket-server
FROM alpine:3.11.3 as server-builder
RUN apk add cabal ghc
RUN apk add ca-certificates
RUN apk add wget
RUN apk add musl-dev
RUN apk add zlib zlib-dev
RUN apk add git
RUN apk add postgresql postgresql-dev
RUN apk add binutils-gold
COPY wrk-websocket-server.cabal cabal.project.local cabal.project cabal.project.freeze /root/wrk-websocket-dir/
WORKDIR /root/wrk-websocket-dir
RUN cabal new-update
RUN cabal new-build --only-dependencies
COPY src/ src/
COPY app/ app/
RUN cabal new-build
RUN cp $(cabal new-exec which wrk-websocket-server) /tmp

# Building wrk2
FROM alpine:3.11.3 as wrk2-builder
ARG tag=print-latencies
RUN apk add alpine-sdk openssl-dev luajit-dev
RUN apk add zlib zlib-dev
RUN wget -O /tmp/wrk2.zip https://github.com/hasura/wrk2/archive/$tag.zip
RUN unzip /tmp/wrk2.zip -d /tmp/
RUN make -C /tmp/wrk2-${tag}

# Install wrk as an alpine package
FROM alpine:3.11.3
ARG tag=print-latencies
ENV HASURA_BENCH_WRK_LUA_SCRIPT=/root/bench_scripts/bench-wrk.lua
ENV HASURA_BENCH_WRK2_LUA_SCRIPT=/root/bench_scripts/bench-wrk2.lua
MAINTAINER nizar@hasura.io
RUN apk add wrk lua5.1-cjson gmp libffi-dev
COPY --from=server-builder /tmp/wrk-websocket-server /usr/bin/wrk-websocket-server
COPY --from=wrk2-builder /tmp/wrk2-$tag/wrk /usr/bin/wrk2
COPY bench_scripts /root/bench_scripts
