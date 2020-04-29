# Auth webhook using python-flask

A boilerplate authentication webhook for Hasura GraphQL Engine written in Python
using Flask.

## Run the webhook

### Clone the repo

```bash
git clone https://github.com/hasura/graphql-engine
cd graphql-engine/community/boilerplates/auth-webhooks/python-flask
```

Run the webhook using any of the tree methods below:

### Run locally

```bash
pip install -r requirements.txt
export FLASK_APP=auth-webhook.py
flask run
```

Webhook will be available at `http://localhost:5000/auth-webhook`

### Using Docker

```bash
docker build -t hasura-auth-webhook .
docker run -p 5000:5000 hasura-auth-webhook
```

Webhook will be available at `http://localhost:5000/auth-webhook`

### Deploy using Now

Install and configure [`now`](https://zeit.co/now):

```bash
npm install -g now
now login
```

Deploy the webhook:

```bash
now
```

Webhook will be available at a url like `https://python-flask-lrnfqprjcc.now.sh`

## Configure Hasura

Configure Hasura with the webhook url. You will need to set an admin secret key to
enable webhook.

When running Hasura as a docker container, `localhost` will point to the
container itself, not the host machine. So, if you're running the webhook
locally or as a container (not on a public url), you'll need to:

1. Use [`docker
   network`](https://docs.docker.com/engine/reference/commandline/network/) and
   keep Hasura and the webhook container in the same network so that webhook url
   will become `http://container-id:5000/auth-webhook` 
2. Linux: Bind both containers on host network (use `--net=host` with docker
   run) so that `localhost` will be the host's network itself. Here, webhook url
   will be `http://localhost:5000/auth-webhook`
3. Mac: If webhook is running on the host, url will be
   `http://host.docker.internal:5000/auth-webhook` 

Set the following environment variables for Hasura:

```
HASURA_GRAPHQL_ADMIN_SECRET=myadminsecretkey
HASURA_GRAPHQL_AUTH_WEBHOOK=http://localhost:5000/auth-webhook
```

All queries will be now validated through the webhook.

> Read more on [authentication and access control](https://hasura.io/docs/1.0/graphql/manual/auth/index.html).
