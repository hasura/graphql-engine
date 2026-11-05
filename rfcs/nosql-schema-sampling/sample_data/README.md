# Sample data

`import.sh` loads MongoDB's `sample_mflix` sample dataset from `sample_mflix/`,
which `../docker-compose.yml` mounts into the MongoDB container.

The dataset itself (~60MB of JSON) has been removed from the repo to keep it
out of everyone's checkout, CI included. It's still in git history; to restore
it, run this from this directory before `docker compose up`:

```sh
git restore --source=f34e1363cc -- sample_mflix
```

This only writes the files to your working tree (not the index), and
`sample_mflix/` is git-ignored, so it won't be committed again by accident.
