# Hasura GraphQL Engine on Heroku

## Install the Hasura CLI

```bash
curl -L https://storage.googleapis.com/hasuractl/install-dev.sh | bash 
```

Once the download is complete, hit ctrl-c before you're prompted for your password and then move the file manually. We're doing this because this is a preview release of the hasura CLI!

```bash
mv /tmp/hasura /usr/local/bin/hasura-dev
```

## Option 1: Deploy on Heroku free tier (recommended for getting started)

### Step 1: Deploy on heroku

[![Deploy](https://www.herokucdn.com/deploy/button.svg)](https://heroku.com/deploy?template=https://github.com/karthikvt26/heroku-push)

### Step 2: Initialise a project directory and open console

```bash
hasura init --directory my-project --endpoint https://HEROKU_APP_NAME.herokuapp.com
cd my-project
hasura console
```

----------------------------------------


## Option 2: Deploy via git

### Step 1: Initialise a project directory

```bash
hasura init --directory my-project
```

### Step 2: Deploy to heroku

```bash
cd my-project
cd __install/heroku
git init
git commit -am 'first commit'
git remote add heroku HEROKU_GIT_REMOTE
git push heroku master
```

### Step 3: Open the hasura console

In the `my-project/config.yaml` file set the endpoint:

```yaml
endpoint: https://HEROKU_APP_NAME.herokuapp.com
```

Now, open the hasura console:

```bash
# Run this command in the my-project/ directory
hasura console
```
