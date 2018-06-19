# Hasura GraphQL Engine using Docker

## Prerequisites

- Install [docker](https://docs.docker.com/install/)


## Install the Hasura CLI

```
curl -L https://storage.googleapis.com/hasuractl/install-dev.sh | bash 
```

Once the download is complete, hit ctrl-c before you're prompted for your password and then move the file manually. We're doing this because this is a preview release of the hasura CLI!

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
