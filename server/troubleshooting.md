### `cabal: Could not resolve dependencies:`

a.k.a "how do I get Cabal to update the freeze file with the dependencies it needs, and why doesn't `cabal update` work?"
- issue: `cabal freeze` also freezes the `index-state` at the bottom of the file so, in some cases, `cabal update` has no effect
- fix: 
  - delete the line starting with `index-state` at the bottom of the `cabal.project.freeze` file
  - delete lines for any dependencies you want to update
  - run `cabal update`/`cabal freeze`
