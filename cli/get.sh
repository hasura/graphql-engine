#!/usr/bin/env bash

# Some helpful functions
yell() { echo -e "${RED}FAILED> $* ${NC}" >&2; }
die() { yell "$*"; exit 1; }
try() { "$@" || die "failed executing: $*"; }
log() { echo -e "--> $*"; }

# Colors for colorizing
RED='\033[0;31m'
GREEN='\033[0;32m'
PURPLE='\033[0;35m'
BLUE='\033[0;34m'
YELLOW='\033[0;33m'
NC='\033[0m'

INSTALL_PATH=${INSTALL_PATH:-"/usr/local/bin"}
NEED_SUDO=0

function maybe_sudo() {
    if [[ "$NEED_SUDO" == '1' ]]; then
        sudo "$@"
    else
        "$@"
    fi
}

# check for curl
hasCurl=$(which curl)
if [ "$?" = "1" ]; then
    die "You need to install curl to use this script."
fi

log "Getting latest version..."

# adapted from https://github.com/openfaas/faas-cli/blob/master/get.sh
version=$(curl -s -f -H 'Content-Type: text/plain' https://releases.hasura.io/graphql-engine?agent=cli-get.sh)
if [ ! $version ]; then
    log "${YELLOW}"
    log "Failed while attempting to install hasura graphql-engine cli. Please manually install:"
    log ""
    log "2. Open your web browser and go to https://github.com/hasura/graphql-engine/releases"
    log "2. Download the cli from latest release for your platform. Name it 'hasura'."
    log "3. chmod +x ./hasura"
    log "4. mv ./hasura /usr/local/bin"
    log "${NC}"
    die "exiting..."
fi

log "Latest version is $version"

# check for existing hasura installation
hasCli=$(which hasura)
if [ "$?" = "0" ]; then
    log ""
    log "${GREEN}You already have the hasura cli at '${hasCli}'${NC}"
    export n=3
    log "${YELLOW}Downloading again in $n seconds... Press Ctrl+C to cancel.${NC}"
    log ""
    sleep $n
fi

# get platform and arch
platform='unknown'
unamestr=`uname`
if [[ "$unamestr" == 'Linux' ]]; then
    platform='linux'
elif [[ "$unamestr" == 'Darwin' ]]; then
    platform='darwin'
fi

if [[ "$platform" == 'unknown' ]]; then
    die "Unknown OS platform"
fi

arch='unknown'
archstr=`uname -m`
if [[ "$archstr" == 'x86_64' ]]; then
    arch='amd64'
else
    arch='386'
fi

# some variables
suffix="-${platform}-${arch}"
targetFile="/tmp/cli-hasura$suffix"

if [ -e $targetFile ]; then
    rm $targetFile
fi

log "${PURPLE}Downloading hasura for $platform-$arch to ${targetFile}${NC}"
url=https://github.com/hasura/graphql-engine/releases/download/$version/cli-hasura$suffix

try curl -L# -f -o $targetFile "$url"
try chmod +x $targetFile

log "${GREEN}Download complete!${NC}"

# check for sudo
needSudo=$(touch ${INSTALL_PATH}/.hasurainstall &> /dev/null)
if [[ "$?" == "1" ]]; then
    NEED_SUDO=1
fi
rm ${INSTALL_PATH}/.hasurainstall &> /dev/null

if [[ "$NEED_SUDO" == '1' ]]; then
    log
    log "${YELLOW}Path '$INSTALL_PATH' requires root access to write."
    log "${YELLOW}This script will attempt to execute the move command with sudo.${NC}"
    log "${YELLOW}Are you ok with that? (y/N)${NC}"
    read a
    if [[ $a == "Y" || $a == "y" || $a = "" ]]; then
        log
    else
        log
        log "  ${BLUE}sudo mv $targetFile ${INSTALL_PATH}/hasura${NC}"
        log
        die "Please move the binary manually using the command above."
    fi
fi

log "Moving cli from $targetFile to ${INSTALL_PATH}"

try maybe_sudo mv $targetFile ${INSTALL_PATH}/hasura

log
log "${GREEN}hasura cli installed to ${INSTALL_PATH}${NC}"
log

if [ -e $targetFile ]; then
    rm $targetFile
fi

hasura version

if ! $(echo "$PATH" | grep -q "$INSTALL_PATH"); then
    log
    log "${YELLOW}$INSTALL_PATH not found in \$PATH, you might need to add it${NC}"
    log 
fi