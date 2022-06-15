# Compile a graphql-engine executable using brew and ghc-8.10.7

*Note: These instructions use the path `/opt/homebrew` in various places, but this path may be different on your machine depending on where you have installed homebrew (for example, many older homebrew installation were installed to `/usr/local`). You can find out the path of your homebrew installation by running the command `brew --prefix`, and if the output is not `/opt/homebrew`, then you should replace any instances of `/opt/homebrew` in these instructions with the path output by `brew --prefix`.*

1.  Install ghc-8.10.7 and cabal-install via [ghcup](https://www.haskell.org/ghcup/)

2.  Install dependencies:

    ```sh
    brew install google-cloud-sdk
    brew install node@14
    brew install openssl
    brew install pcre
    brew install unixodbc
    brew install libpq
    brew install mysql-client@5.7
    brew install libffi
    brew install llvm@11
    brew install microsoft/mssql-release/mssql-tools
    ```

     And add them to your environment:

    ```sh
    echo 'export PATH="/opt/homebrew/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/bin:$PATH"' >> ~/.zshrc
    echo 'export PATH="/opt/homebrew/opt/openssl@1.1/bin:$PATH"' >> ~/.zshrc
    echo 'export PATH="/opt/homebrew/opt/node@14/bin:$PATH"' >> ~/.zshrc
    echo 'export PATH="/opt/homebrew/opt/mysql-client@5.7/bin:$PATH"' >> ~/.zshrc
    echo 'export PATH="/opt/homebrew/opt/libpq/bin:$PATH"' >> ~/.zshrc
    echo 'export PATH="/opt/homebrew/opt/llvm@11/bin:$PATH"' >> ~/.zshrc
    ```


3. Install console assets

   This step may require you to have python2 installed and available in your $PATH.

   ```sh
   cd console
   npm ci
   npm run server-build
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
    package mysql
      extra-include-dirs:
        /opt/homebrew/opt/openssl/include
        /opt/homebrew/opt/mysql-client@5.7/include
      extra-lib-dirs:
        /opt/homebrew/opt/openssl/lib
        /opt/homebrew/opt/mysql-client@5.7/lib

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
    For example if you are building `v2.6.1` then you can run the following command:

    ```sh
    echo '2.6.1' > server/CURRENT_VERSION
    ```

    This version number is used for the output of `graphql-engine --version` in your compiled binary, and it also used when fetching the frontend assets for the console from the CDN when running the server (if you do not specify `--console-assets-dir` to make it load from a locally compiled version).

7.  Building the server should now work:

    ```sh
    cabal v2-update
    cabal v2-build exe:graphql-engine -j4
    ```
