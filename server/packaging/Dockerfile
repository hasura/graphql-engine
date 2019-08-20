ARG stack_resolver
ARG packager_version

# Stage 1: build the application
FROM fpco/stack-build:${stack_resolver} as builder
ARG stack_flags=""
ARG build_flags=""
ARG project="graphql-engine"
RUN mkdir /build
COPY . /build/
WORKDIR /build
RUN stack --system-ghc ${stack_flags} build ${build_flags}
RUN cp $(stack --system-ghc path --dist-dir)/build/${project}/${project} /build

# Stage 2: package the app into a busybox rootfs, strip, compress etc.
FROM hasura/graphql-engine-packager:${packager_version} as packager
ARG project="graphql-engine"
RUN mkdir -p /packaging/build/rootfs
COPY --from=builder /build/${project} /root/
RUN /build.sh ${project} | tar -xC /packaging/build/rootfs
RUN strip --strip-unneeded /packaging/build/rootfs/bin/${project}
RUN upx /packaging/build/rootfs/bin/${project}

# Stage 3: copy the rootfs into a scratch container
FROM scratch
STOPSIGNAL SIGINT
COPY --from=packager /packaging/build/rootfs /
