# Mysql + Hasura

Hasura GraphQL Engine is a blazing-fast GraphQL Server that we now have mysql support! Currently we're in *prototype* so not all features are implemented yet. The following are further details on use, what features are currently available, and some roadmap details.

## Prototype Feature Goals

* Initial support goal is around MySQL 8+.
* Prototype release will cover basic CRUD with JSON aggs.
* Support of MySQL as a database and dynamic source to add at runtime.
* No need for metadata or execution of DDL against the MySQL to function.
* CRUD, Realtime, and event trigger features.

## Try It Out

![Hasura + Mysql](assets/mysql.png)

Check out the full repo for more details [here](https://github.com/hasura/graphql-engine).

Get started with Mysql & Hasura.

1. Get the docker-compose file for the mysql prototype examples.

```shell
# in a new directory run
wget https://raw.githubusercontent.com/hasura/graphql-engine/LOCATIONOFDOCKERCOMPOSEFILE/docker-compose.yaml
# or run
curl https://raw.githubusercontent.com/hasura/graphql-engine/LOCATIONOFDOCKERCOMPOSEFILE/docker-compose.yaml -o docker-compose.yml
```

Now run docker-compose to bring up the containers.

```shell
$ docker-compose up -d
```

Verify the containers are running.

```shell
$ docker ps
```

Now open a browser and navigate to `http://localhost:808/console` to try out your newly deployed Hasura with mysql support.

# Questions, Comments, Help, Documentation, and Support

* [Questions, join us on Discord](https://discordapp.com/invite/hasura)
* [Docs](https://hasura.io/docs)
* []()
* [Issues](https://github.com/hasura/graphql-engine/issues)

[GET NOTIFIED LINK / BUTTON HERE](??)