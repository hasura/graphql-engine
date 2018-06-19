# Hasura GraphQL Engine using Docker

## Prerequisites

- Install [docker](https://docs.docker.com/install/)


## Install the Hasura CLI

```
curl -L https://storage.googleapis.com/hasuractl/install-dev.sh | bash 
```

If you already have Hasura installed, then once the download is complete, hit ctrl-c before you're prompted for your password. And then move the file manually:

```
mv /tmp/hasura /usr/local/bin/hasura-dev
```


## Deploy postgres (optional)

- Start a Postgres container if you don't have postgres running already:
  ```bash
  docker run -p 5432:5432  -d postgres
  ```

## Deploy

- Start Hasura GraphQL Engine container and link to Postgres with the `database-url` flag:

#### Mac
  ```bash
  docker run -p 8080:8080 \
             hasuranightly/raven:8df5234 \
             raven \
             --database-url postgres://postgres:@host.docker.internal:5432/postgres \
             serve
  ```

#### Linux
  ```bash
  docker run -p 8080:8080 \
             hasuranightly/raven:8df5234 \
             raven \
             --database-url postgres://postgres:@$DOCKER_HOST_IP:5432/postgres \
             serve
  ```

**Note:** Change the database-url value to your own postgres if `postgres://username:password@postgres:5432/dbname` or if you have no password `postgres://username:@postgres:5432/dbname`
 
## Set up your project
 
```bash
$ mkdir my-project
$ cd my-project
$ echo 'endpoint: http://localhost:8080' > config.yaml
```

## Open the console

Open the console and start exploring APIs / managing tables/views:

```bash
hasura-dev console
```
