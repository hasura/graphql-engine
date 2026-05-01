#!/usr/bin/env bash
# Build a runnable Docker image of graphql-engine + console for local validation.
#
# Produces an arm64 image that runs natively on Apple Silicon. Uses dynamic
# linking + unoptimized graphql-engine to dodge a heap-overflow at link time
# inside the default Docker Desktop VM. The CI build (linux/amd64 on a real
# Linux runner) does not need any of these workarounds.
#
# Reuses cached cabal store + dist-newstyle across runs via Docker volumes,
# so only the first run pays the ~30-45 min cabal cost.

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
PLATFORM="${PLATFORM:-linux/arm64}"
TAG="${TAG:-local/graphql-engine:$(git -C "$REPO_ROOT" rev-parse --abbrev-ref HEAD)-${PLATFORM##*/}}"
BASE_TAG="${BASE_TAG:-hasura/graphql-engine-base:local-${PLATFORM##*/}}"
BUILDER_TAG="${BUILDER_TAG:-hasura-builder:local-${PLATFORM##*/}}"
NODE_VERSION="${NODE_VERSION:-16.15.1}"

step() { printf '\n=== %s ===\n' "$*"; }

# Always restore the symlink on exit; the container temporarily repoints it.
ORIGINAL_LOCAL_TARGET=""
if [ -L "$REPO_ROOT/cabal.project.local" ]; then
  ORIGINAL_LOCAL_TARGET="$(readlink "$REPO_ROOT/cabal.project.local")"
fi
restore_symlink() {
  if [ -n "$ORIGINAL_LOCAL_TARGET" ]; then
    ln -sfn "$ORIGINAL_LOCAL_TARGET" "$REPO_ROOT/cabal.project.local"
  fi
}
trap restore_symlink EXIT

# ---------------------------------------------------------------------------
step "1/6 Console assets (host node $NODE_VERSION)"
# ---------------------------------------------------------------------------
if [ -d "$REPO_ROOT/frontend/dist/apps/server-assets-console-ce" ] && [ -z "${REBUILD_CONSOLE:-}" ]; then
  echo "skipping (set REBUILD_CONSOLE=1 to force)"
else
  # shellcheck disable=SC1090
  source "${NVM_DIR:-$HOME/.nvm}/nvm.sh"
  nvm use "$NODE_VERSION"
  cd "$REPO_ROOT/frontend"
  node .yarn/releases/yarn-3.2.4.cjs install --immutable
  node .yarn/releases/yarn-3.2.4.cjs server-build:ce
  cd "$REPO_ROOT"
fi

# ---------------------------------------------------------------------------
step "2/6 Builder image"
# ---------------------------------------------------------------------------
GHC_VERSION="$(cat "$REPO_ROOT/.ghcversion")"
BUILDER_DOCKERFILE="$REPO_ROOT/scripts/build-local-image.builder.dockerfile"
cat > "$BUILDER_DOCKERFILE" <<'DOCKERFILE'
FROM ubuntu:jammy-20250530
ARG GHC_VERSION
ENV LANG=C.UTF-8 LC_ALL=C.UTF-8 DEBIAN_FRONTEND=noninteractive

RUN set -ex; \
    apt-get update; \
    apt-get install -y \
      build-essential curl gnupg2 ca-certificates lsb-release pkg-config git \
      libpq-dev libssl-dev libkrb5-dev libnuma-dev libpcre3-dev unixodbc-dev \
      zlib1g-dev libgmp-dev libffi-dev libtinfo-dev libsystemd-dev libreadline-dev \
      python3 xz-utils

RUN set -ex; \
    curl -fsS "https://packages.microsoft.com/config/ubuntu/$(lsb_release -rs)/prod.list" \
      > /etc/apt/sources.list.d/mssql-release.list; \
    curl -fsS 'https://packages.microsoft.com/keys/microsoft.asc' | apt-key add -; \
    apt-get update; \
    ACCEPT_EULA=Y apt-get install -y msodbcsql18

ENV GHCUP_INSTALL_BASE_PREFIX=/opt PATH=/opt/.ghcup/bin:$PATH
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org \
      | BOOTSTRAP_HASKELL_NONINTERACTIVE=1 \
        BOOTSTRAP_HASKELL_GHC_VERSION="$GHC_VERSION" \
        BOOTSTRAP_HASKELL_CABAL_VERSION=3.12.1.0 \
        BOOTSTRAP_HASKELL_INSTALL_NO_STACK=1 \
        BOOTSTRAP_HASKELL_ADJUST_BASHRC=N \
        sh

WORKDIR /src
DOCKERFILE
docker buildx build --platform "$PLATFORM" --load \
  --build-arg "GHC_VERSION=$GHC_VERSION" \
  -t "$BUILDER_TAG" -f "$BUILDER_DOCKERFILE" "$(dirname "$BUILDER_DOCKERFILE")"

# ---------------------------------------------------------------------------
step "3/6 Linux binary (cabal in container, may take 30-45 min on first run)"
# ---------------------------------------------------------------------------
docker volume create hasura-cabal-store >/dev/null
docker volume create hasura-cabal-dist >/dev/null

docker run --rm --platform "$PLATFORM" \
  -v "$REPO_ROOT:/src" \
  -v hasura-cabal-store:/root/.cabal \
  -v hasura-cabal-dist:/src/dist-newstyle-linux \
  -w /src "$BUILDER_TAG" \
  bash -c '
    set -e
    rm -f /src/cabal.project.local
    ln -sf cabal/linux-build.project.local cabal.project.local
    cabal update
    cabal build --builddir=/src/dist-newstyle-linux exe:graphql-engine
    cabal list-bin --builddir=/src/dist-newstyle-linux exe:graphql-engine
  '

# ---------------------------------------------------------------------------
step "4/6 Assemble rootfs"
# ---------------------------------------------------------------------------
ROOTFS="$REPO_ROOT/packaging/graphql-engine/rootfs"
rm -rf "$ROOTFS"
mkdir -p "$ROOTFS/bin" "$ROOTFS/lib" "$ROOTFS/srv"
cp -r "$REPO_ROOT/frontend/dist/apps/server-assets-console-ce" "$ROOTFS/srv/console-assets"

# Walk ldd recursively inside the container, copy every Hasura-built and
# cabal-store .so into rootfs/lib, plus the GHC RTS .so files.
docker run --rm --platform "$PLATFORM" \
  -v "$REPO_ROOT:/src" \
  -v hasura-cabal-store:/root/.cabal \
  -v hasura-cabal-dist:/src/dist-newstyle-linux \
  -w /src "$BUILDER_TAG" \
  bash -c '
    set -e
    BIN=$(cabal list-bin --builddir=/src/dist-newstyle-linux exe:graphql-engine)
    cp "$BIN" /src/packaging/graphql-engine/rootfs/bin/graphql-engine
    chmod +x /src/packaging/graphql-engine/rootfs/bin/graphql-engine

    seen=$(mktemp); queue=$(mktemp); echo "$BIN" > "$queue"
    while [ -s "$queue" ]; do
      lib=$(head -1 "$queue"); tail -n +2 "$queue" > "$queue.new" && mv "$queue.new" "$queue"
      grep -qxF "$lib" "$seen" 2>/dev/null && continue
      echo "$lib" >> "$seen"
      ldd "$lib" 2>/dev/null | awk "/=>/ {print \$3}" | while read -r dep; do
        case "$dep" in
          /root/.cabal/store/*|/src/dist-newstyle-linux/*) echo "$dep" >> "$queue" ;;
        esac
      done
    done

    grep -E "^(/root/.cabal/store|/src/dist-newstyle-linux)" "$seen" | sort -u | \
      while read -r so; do cp "$so" /src/packaging/graphql-engine/rootfs/lib/; done

    GHC_LIBDIR=$(ghc --print-libdir)
    find "$GHC_LIBDIR" \( -name "libHS*.so" -o -name "libffi*.so*" \) \
      -exec cp -P {} /src/packaging/graphql-engine/rootfs/lib/ \;

    cd /src/packaging/graphql-engine
    missing=$(LD_LIBRARY_PATH=rootfs/lib ldd rootfs/bin/graphql-engine 2>&1 | grep -c "not found" || true)
    echo "Missing libs: $missing"
    [ "$missing" = "0" ] || { echo "ERROR: unresolved libs"; exit 1; }
  '

# ---------------------------------------------------------------------------
step "5/6 Runtime base image"
# ---------------------------------------------------------------------------
docker buildx build --platform "$PLATFORM" --load \
  -t "$BASE_TAG" \
  -f "$REPO_ROOT/packaging/graphql-engine-base/ubuntu.dockerfile" \
  "$REPO_ROOT/packaging/graphql-engine-base"

# ---------------------------------------------------------------------------
step "6/6 Final image"
# ---------------------------------------------------------------------------
LOCAL_DOCKERFILE="$REPO_ROOT/packaging/graphql-engine/local.dockerfile"
cat > "$LOCAL_DOCKERFILE" <<'DOCKERFILE'
ARG BASE_IMAGE
FROM ${BASE_IMAGE}
ARG HGE_BINARY_NAME=graphql-engine
ENV HGE_BINARY=${HGE_BINARY_NAME}
COPY rootfs/bin/ /bin/
COPY rootfs/srv/ /srv/
COPY rootfs/lib/ /usr/local/lib/hasura/
RUN echo "/usr/local/lib/hasura" > /etc/ld.so.conf.d/hasura.conf && ldconfig
HEALTHCHECK --start-period=10s CMD curl -f http://localhost:8080/healthz || exit 1
CMD ["sh", "-c", "exec ${HGE_BINARY} serve"]
DOCKERFILE

docker buildx build --platform "$PLATFORM" --load \
  --build-arg "BASE_IMAGE=$BASE_TAG" \
  -t "$TAG" \
  -f "$LOCAL_DOCKERFILE" \
  "$REPO_ROOT/packaging/graphql-engine"

cat <<EOF

Built image: $TAG

Smoke test:
  docker network create hasura-test
  docker run -d --rm --platform $PLATFORM --name hasura-test-pg --network hasura-test \\
    -e POSTGRES_PASSWORD=postgres -e POSTGRES_DB=hasura postgres:15
  sleep 5
  docker run -d --rm --platform $PLATFORM --name hasura-test-engine --network hasura-test \\
    -p 18080:8080 \\
    -e HASURA_GRAPHQL_DATABASE_URL=postgres://postgres:postgres@hasura-test-pg:5432/hasura \\
    -e HASURA_GRAPHQL_ENABLE_CONSOLE=true \\
    -e HASURA_GRAPHQL_CONSOLE_ASSETS_DIR=/srv/console-assets \\
    $TAG
  curl http://localhost:18080/healthz
  open http://localhost:18080/console
EOF
