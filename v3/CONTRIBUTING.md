# V3 Contributing Guide

## Getting Started

### Using Docker

Start a development container (which includes NDC agents for testing):

```
docker compose run --build --rm dev_setup bash
```

### Without Docker

You will need to install some packages:

- The Rust compiler
- `protobuf-compiler`

For development, you may need to install some additional tools such as
`nextest`. See the [Dockerfile](Dockerfile).

### Building the source

If the dependencies are correctly installed as above, you should now be able to
run

```
cargo build
```

From here, you can follow the instructions in <README.md> to set up a working
server.

## Installing project dependencies with Nix

We maintain a [Nix Flake](flake.nix) that will install all of the project
dependencies required to work on `v3-engine`.

To use it:

1. [Install Nix](https://github.com/DeterminateSystems/nix-installer)
2. Run `nix develop` in the project root. This starts a new shell which exposes
   the Nix project dependencies on your `$PATH`, and can be exited with `exit`.
3. Type `echo $IN_NIX_SHELL` to confirm you're in a Nix shell. It should print
   "impure".
4. Run `which rustc` to verify that the Rust compiler is being provided by Nix.
   You should see a path starting with `/nix/store/` and ending in `/bin/rustc`.

We also supply configuration for [direnv](https://direnv.net/) to allow
autoloading the Nix dependencies upon entering the project folder.

To use this:

1. Install direnv:
1. [Install the direnv program](https://direnv.net/docs/installation.html)
1. [Add the direnv hook to your shell](https://direnv.net/docs/hook.html)
1. Open a new terminal
1. `cp .envrc.local.example .envrc.local`
1. Run `direnv allow`
1. Nix should now install project dependencies and put them in your `$PATH`
   whenever you enter the project folder.
1. Type `echo $IN_NIX_SHELL` to confirm you're in a Nix shell. It should print
   "impure".
1. Run `which rustc` to verify that the Rust compiler is being provided by Nix.
   You should see a path starting with `/nix/store/` and ending in `/bin/rustc`.

## Additional Reading

- [Architecture](docs/architecture.md)
- [Roles and Annotations](docs/roles-and-annotations.md)
- [Errors](docs/errors.md)
