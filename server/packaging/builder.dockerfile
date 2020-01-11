FROM haskell:8.6.5

# Install brotli dependencies
RUN apt-get -y update \
    && apt-get -y install cmake pkgconf \
    && git clone https://github.com/google/brotli.git && cd brotli && mkdir out && cd out && ../configure-cmake \
    && make && make test && make install && ldconfig
