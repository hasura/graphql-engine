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

## Deploy

- Start Hasura GraphQL Engine container and link to Postgres with the `database-url` flag:

  ```bash
  docker run -p 8080:8080 \
             hasuranightly/raven:8df5234 \
             raven \
             --database-url postgres://username:password@hostname:port/dbname \
             serve
  ```

#### Examples of `database-url`

- If the username and database is called admin: `postgres://admin:password@localhost:5432/admin`
- If there is no password: `postgres://admin:@localhost:5432/admin`

## Set up your project

Run this command: 

```bash
hasura init --endpoint http://localhost:8080 --directory my-project
```

## Open the console

Open the console and start exploring GraphQL APIs & managing tables/views:

```bash
cd my-project
hasura-dev console
```
