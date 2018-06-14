# Hasura GraphQL Engine on Heroku

## Prerequisites

- Heroku CLI
- Heroku Account
- Docker

## Setting up

- Create a Heroku App
```
$ heroku create
Creating app... done, ⬢ thawing-savannah-75305
https://thawing-savannah-75305.herokuapp.com/ | https://git.heroku.com/thawing-savannah-75305.git
```
Note the app name `thawing-savannah-75305` from the command above. 
In the next commands, substitute `<app-name>` with `thawing-savannah-75305`

- Create a Heroku Postgres Addon for this app
```bash
$ heroku addons:create heroku-postgresql:hobby-dev -a thawing-savannah-75305
```

## Deploy

- Switch to the `heroku/setup` directory. You should have a `Dockerfile` when you hit `ls`:
  ```bash
  $ ls
  Dockerfile README.md
  ```
- Make sure you're logged in to heroku's container registry:
  ```
  $ heroku container:login
  ```
- Execute the following commands to deploy
  ```bash
  heroku container:push web -a <app-name>
  heroku container:release web -a <app-name>
  ```
- Check the logs to verify if everything is ok:
  ```bash
  heroku logs -a <app-name>
  ```

## Configure

(Assuming you have already executed `hasura init`)

- Edit `config.yaml` and add `endpoint` as the Heroku App URL
  ```yaml
  endpoint: https://<app-name>.herokuapp.com
  ```

## Console

Open the console and start exploring APIs / manage tables/views:
```bash
hasura console
```
