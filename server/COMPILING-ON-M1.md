# Compile a native arm64 graphql-engine executable on M1 using brew and ghc-8.10.7

1.  Install ghc-8.10.7 and cabal-install via ghcup

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

    cd server
    python3 -m venv .python-venv
    source .python-venv/bin/activate
    pip3 install -r tests-py/requirements.txt
    (cd tests-py/remote_schemas/nodejs && npm ci)
    ```

5.  Add the C dependencies in the `cabal.project.dev-sh.local` and `cabal.project.local` files (according to where the dependencies were installed by homebrew):

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

    Note: If you don't have special requirements of a `cabal.project.local` you can just add a symbolic link to `cabal.project.dev-sh.local`:

    ```sh
    ln -s cabal.project.dev-sh.local cabal.project.local
    ```


6.  Building the server should now work:

    ```sh
    cabal v2-update
    cabal v2-build graphql-engine -j4
    ```
