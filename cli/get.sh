# adapted from https://github.com/openfaas/faas-cli/blob/master/get.sh
version=$(curl -s -H 'Content-Type: text/plain' https://releases.hasura.io/graphql-engine?agent=cli-get.sh)
if [ ! $version ]; then
    echo "Failed while attempting to install hasura graphql-engine cli. Please manually install:"
    echo ""
    echo "1. Open your web browser and go to https://github.com/hasura/graphql-engine/releases"
    echo "2. Download the cli from latest release for your platform. Call it 'hasura'."
    echo "3. chmod +x ./hasura"
    echo "4. mv ./hasura /usr/local/bin"
    exit 1
fi

hasCli() {
 
    has=$(which hasura)

    if [ "$?" = "0" ]; then
        echo
        echo "You already have the hasura cli!"
        export n=3
        echo "Overwriting in $n seconds.. Press Control+C to cancel."
        echo
        sleep $n
    fi

    hasCurl=$(which curl)
    if [ "$?" = "1" ]; then
        echo "You need curl to use this script."
        exit 1
    fi
}


getPackage() {
    uname=$(uname)
    userid=$(id -u)

    suffix=""
    case $uname in
    "Darwin")
    suffix="-darwin-amd64"
    ;;
    "Linux")
        arch=$(uname -m)
        case $arch in
        "amd64" | "x86_64")
        suffix="-linux-amd64"
        ;;
        esac
        case $arch in
        "aarch64")
        suffix="-linux-arm64"
        ;;
        esac
        case $arch in
        "armv6l" | "armv7l")
        suffix="-linux-armhf"
        ;;
        esac
    ;;
    esac

    targetFile="/tmp/cli-hasura$suffix"
    
    if [ "$userid" != "0" ]; then
        targetFile="$(pwd)/cli-hasura$suffix"
    fi

    if [ -e $targetFile ]; then
        rm $targetFile
    fi

    url=https://github.com/hasura/graphql-engine/releases/download/$version/cli-hasura$suffix
    echo "Downloading package $url as $targetFile"

    curl -sSL $url --output $targetFile

    if [ "$?" = "0" ]; then

    chmod +x $targetFile

    echo "Download complete."

        if [ "$userid" != "0" ]; then
            
            echo
            echo "=========================================================" 
            echo "==    As the script was run as a non-root user the     =="
            echo "==    following command may need to be run manually    =="
            echo "========================================================="
            echo
            echo "  sudo cp cli-hasura$suffix /usr/local/bin/hasura"
            echo

        else

            echo
            echo "Running as root - Attempting to move hasura cli to /usr/local/bin"

            mv $targetFile /usr/local/bin/hasura
        
            if [ "$?" = "0" ]; then
                echo "New version of hasura cli installed to /usr/local/bin"
            fi

            if [ -e $targetFile ]; then
                rm $targetFile
            fi

            hasura version
        fi
    fi
}

hasCli
getPackage