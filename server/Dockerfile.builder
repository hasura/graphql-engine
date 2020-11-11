FROM haskell:8.10.1-buster as builder

ENV LANG=C.UTF-8 LC_ALL=C.UTF-8

RUN apt-get update \
  && apt-get install -y upx libpq-dev libkrb5-dev libssl-dev
