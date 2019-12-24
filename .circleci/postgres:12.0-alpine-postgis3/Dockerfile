# See https://github.com/appropriate/docker-postgis/issues/157#issuecomment-562812857

FROM circleci/postgres:12.0-alpine

ENV POSTGIS_VERSION 3.0.0

RUN set -ex \
    \
    && apk add --no-cache --virtual .fetch-deps ca-certificates openssl tar \
    \
    && wget -O postgis.tar.gz "https://github.com/postgis/postgis/archive/$POSTGIS_VERSION.tar.gz" \
    && mkdir -p /usr/src/postgis \
    && tar \
        --extract \
        --file postgis.tar.gz \
        --directory /usr/src/postgis \
        --strip-components 1 \
    && rm postgis.tar.gz \
    \
    && apk add --no-cache --virtual .build-deps \
        autoconf automake json-c-dev libtool libxml2-dev make perl llvm clang clang-dev \
    \
    && apk add --no-cache --virtual .build-deps-edge \
        --repository http://dl-cdn.alpinelinux.org/alpine/edge/community \
        --repository http://dl-cdn.alpinelinux.org/alpine/edge/main \
        g++ gdal-dev geos-dev proj-dev protobuf-c-dev \
    && cd /usr/src/postgis \
    && ./autogen.sh \
    && ./configure \
    && make \
    && make install \
    && apk add --no-cache --virtual .postgis-rundeps \
        json-c \
    && apk add --no-cache --virtual .postgis-rundeps-edge \
        --repository http://dl-cdn.alpinelinux.org/alpine/edge/community \
        --repository http://dl-cdn.alpinelinux.org/alpine/edge/main \
        geos gdal proj protobuf-c libstdc++ \
    && cd / \
    && rm -rf /usr/src/postgis \
    && apk del .fetch-deps .build-deps .build-deps-edge

ADD https://raw.githubusercontent.com/appropriate/docker-postgis/master/initdb-postgis.sh /docker-entrypoint-initdb.d/postgis.sh 
ADD https://raw.githubusercontent.com/appropriate/docker-postgis/master/update-postgis.sh /usr/local/bin/update-postgis.sh

RUN chmod +r /docker-entrypoint-initdb.d/postgis.sh && chmod +rx /usr/local/bin/update-postgis.sh