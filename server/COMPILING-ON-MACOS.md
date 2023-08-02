# Compile a graphql-engine executable using brew and ghc-9.4.5

*Note: These instructions use the path `/opt/homebrew` in various places, but this path may be different on your machine depending on where you have installed homebrew (for example, many older homebrew installation were installed to `/usr/local`). You can find out the path of your homebrew installation by running the command `brew --prefix`, and if the output is not `/opt/homebrew`, then you should replace any instances of `/opt/homebrew` in these instructions with the path output by `brew --prefix`.*

1.  Install ghc-9.4.5 and cabal-install via [ghcup](https://www.haskell.org/ghcup/).

2.  Install dependencies:

    ```sh
    brew install google-cloud-sdk \
                 node@16 \
                 openssl \
                 pcre \
                 unixodbc \
                 libpq \
                 libffi \
                 microsoft/mssql-release/mssql-tools18 \
                 direnv \
                 coreutils \
                 pcre
    ```

     And add them to your environment:

    ```sh
    echo 'export PATH="/opt/homebrew/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/bin:$PATH"' >> ~/.zshrc
    echo 'export PATH="/opt/homebrew/opt/openssl@1.1/bin:$PATH"' >> ~/.zshrc
    echo 'export PATH="/opt/homebrew/opt/node@16/bin:$PATH"' >> ~/.zshrc
    echo 'export PATH="/opt/homebrew/opt/libpq/bin:$PATH"' >> ~/.zshrc
    ```

If you are re-running this command to update your Mac, you may need to run
`cabal clean` before things start working again.

3. Install console assets

   This step may require you to have python2 installed and available in your $PATH.

   ```sh
   cd frontend
   npm ci
   npm run server-build:ce
   cd ..
   ```

4.  Install python

    ```sh
    export PKG_CONFIG_PATH="/opt/homebrew/opt/libffi/lib/pkgconfig"
    export LDFLAGS="-L/opt/homebrew/opt/openssl@1.1/lib"
    export CPPFLAGS="-I/opt/homebrew/opt/openssl@1.1/include"

    cd server
    python3 -m venv .python-venv
    source .python-venv/bin/activate
    pip3 install -r tests-py/requirements.txt
    (cd tests-py/remote_schemas/nodejs && npm ci)
    ```

5.  Append lines below to `cabal/dev-sh.project.local` to allow Cabal (the Haskell build tool) to find the C dependencies you installed earlier (remembering to replace `/opt/homebrew` with your brew prefix if different):

    ```sh
    package odbc
      extra-include-dirs: /opt/homebrew/opt/unixodbc/include
      extra-lib-dirs: /opt/homebrew/opt/unixodbc/lib

    package pcre-light
      extra-include-dirs: /opt/homebrew/opt/pcre/include
      extra-lib-dirs: /opt/homebrew/opt/pcre/lib

    package postgresql-libpq
      extra-include-dirs:
        /opt/homebrew/opt/libpq/include
        /opt/homebrew/opt/openssl/include
      extra-lib-dirs:
        /opt/homebrew/opt/libpq/lib
        /opt/homebrew/opt/openssl/lib

    package pg-client
      extra-include-dirs:
        /opt/homebrew/opt/libpq/include
        /opt/homebrew/opt/openssl/include
      extra-lib-dirs:
        /opt/homebrew/opt/libpq/lib
        /opt/homebrew/opt/openssl/lib
    ```

    Then either copy and paste the entirety of `cabal/dev-sh.project.local` into `cabal.project.local`, or create a symlink by running the command:

     ```sh
     ln -s cabal/dev-sh.project.local cabal.project.local
     ```

     (Copying and pasting allows you to add local projects overrides, which may be needed if you are are planning to make changes to the graphql-engine code, but is not required for simply compiling the code as-is).

6. Write the version number of the graphql-server that you are intending to build to the file `server/CURRENT_VERSION`.
    For example if you are building `v2.13.0` then you can run the following command:

    ```sh
    echo '2.13.0' > server/CURRENT_VERSION
    ```

    This version number is used for the output of `graphql-engine --version` in your compiled binary, and it also used when fetching the frontend assets for the console from the CDN when running the server (if you do not specify `--console-assets-dir` to make it load from a locally compiled version).

7.  Building the server should now work:

    ```sh
    cabal update
    cabal build exe:graphql-engine -j4
    ```
