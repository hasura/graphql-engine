# Contributing

This guide explains how to set up the graphql-engine server for development on your
own machine and how to contribute.

## Pre-requisites

- [stack](https://docs.haskellstack.org/en/stable/README/#how-to-install)
- [Node.js](https://nodejs.org/en/) (>= v8.9)
- npm >= 5.7
- libpq-dev
- python >= 3.5 with pip3

The last two prerequisites can be installed on Debian with:

    $ sudo apt install libpq-dev python3 python3-pip python3-venv

### Upgrading npm

If your npm is too old (>= 5.7 required):

    $ npm install -g npm@latest   # sudo may be required

or update your nodejs.


## Development workflow

You should fork the repo on github and then `git clone https://github.com/<your-username>/graphql-engine`.
After making your changes

### Compile

...console assets:

    $ cd console
    $ npm ci
    $ npm run server-build
    $ cd ..

...and the server:

    $ cd server
    $ stack build --fast

### Run and Test

The easiest way to run `graphql-engine` locally for development is to first
launch a new postgres container with:

    $ scripts/dev.sh postgres

Then in a new terminal launch `graphql-engine` in dev mode with:

    $ scripts/dev.sh graphql-engine

The `dev.sh` will print some helpful information and logs from both services
will be printed to screen.

You can run the test suite with:

    $ scripts/dev.sh test

This should run in isolation.


### Create Pull Request
- Make sure your commit messages meet the [guidelines](../CONTRIBUTING.md).
- Create a pull request from your forked repo to the main repo.
- Every pull request will automatically build and run the tests.

## Code conventions

This helps enforce a uniform style for all committers.

- Compiler warnings are turned on, make sure your code has no warnings.
- Use [hlint](https://github.com/ndmitchell/hlint) to make sure your code has no warnings.
- Use [stylish-haskell](https://github.com/jaspervdj/stylish-haskell) to format your code.
