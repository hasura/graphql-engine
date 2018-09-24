# Firebae to GraphQL

This is A CLI tool to get instant GraphQL API over your Firebase JSON data dump.

[![oclif](https://img.shields.io/badge/cli-oclif-brightgreen.svg)](https://oclif.io)
[![Version](https://img.shields.io/npm/v/firebase2graphql.svg)](https://npmjs.org/package/firebase2graphql)

## Quick start

1. Quickly get the GraphQL Engine running by clicking this button:

   [![Deploy to heroku](https://www.herokucdn.com/deploy/button.svg)](https://heroku.com/deploy?template=https://github.com/hasura/graphql-engine-heroku)

  Note the URL. It will be of the form: `https://<app-name>.herokuapp.com`

2. Export your firebase realtime database as JSON. It will look something like:

    ```json
    {
      "posts" : {
        "-LMbLFOAW2q6GO1bD-5g" : {
          "author" : "Rishichandra Wawhal",
          "authorPic" : "https://lh4.googleusercontent.com/-vPOIBOxCUpo/AAAAAAAAAAI/AAAAAAAAAFo/SKk9hpOB7v4/photo.jpg",
          "body" : "My first post content\nAnd body\nANd structure",
          "starCount" : 0,
          "title" : "My first post",
          "uid" : "4UPmbcaqZKT2NdAAqBahXj4tHYN2"
        },
        "-LMbLIv6VKHYul7p_PZ-" : {
          "author" : "Rishichandra Wawhal",
          "authorPic" : "https://lh4.googleusercontent.com/-vPOIBOxCUpo/AAAAAAAAAAI/AAAAAAAAAFo/SKk9hpOB7v4/photo.jpg",
          "body" : "AKsdjak\naklsdjaskldjklas\nasdklfjaklsdfjklsda\nasdklfjasklf",
          "starCount" : 0,
          "title" : "Whatta proaaa",
          "uid" : "4UPmbcaqZKT2NdAAqBahXj4tHYN2"
        }
      },
      "user-posts" : {
        "4UPmbcaqZKT2NdAAqBahXj4tHYN2" : {
          "-LMbLFOAW2q6GO1bD-5g" : {
            "author" : "Rishichandra Wawhal",
            "authorPic" : "https://lh4.googleusercontent.com/-vPOIBOxCUpo/AAAAAAAAAAI/AAAAAAAAAFo/SKk9hpOB7v4/photo.jpg",
            "body" : "My first post content\nAnd body\nANd structure",
            "starCount" : 0,
            "title" : "My first post",
            "uid" : "4UPmbcaqZKT2NdAAqBahXj4tHYN2"
          },
          "-LMbLIv6VKHYul7p_PZ-" : {
            "author" : "Rishichandra Wawhal",
            "authorPic" : "https://lh4.googleusercontent.com/-vPOIBOxCUpo/AAAAAAAAAAI/AAAAAAAAAFo/SKk9hpOB7v4/photo.jpg",
            "body" : "AKsdjak\naklsdjaskldjklas\nasdklfjaklsdfjklsda\nasdklfjasklf",
            "starCount" : 0,
            "title" : "Whatta proaaa",
            "uid" : "4UPmbcaqZKT2NdAAqBahXj4tHYN2"
          }
        }
      },
      "users" : {
        "4UPmbcaqZKT2NdAAqBahXj4tHYN2" : {
          "email" : "rishichandrawawhal@gmail.com",
          "profile_picture" : "https://lh4.googleusercontent.com/-vPOIBOxCUpo/AAAAAAAAAAI/AAAAAAAAAFo/SKk9hpOB7v4/photo.jpg",
          "username" : "Rishichandra Wawhal"
        }
      }
    }
 
    ```

3. Use the CLI to import the data:

    ```
    $ npm install -g firebase2graphql
    $ firebase2graphql https://<app-name>.herokuapp.com --db=./path/to/db.json
    ```

4. That's it. You can go your GraphQL Engine URL `https://<app-name>.herokuapp.com` and start querying this data over GraphQL:

    ```graphql
    query {
      posts (
        order_by: title_asc,
        limit: 10
      ) {
        __id
        title
        body
        author
      }
    }
    ```


## Installation

### CLI

```bash
npm install -g firebase2graphql
```

## Usage

### CLI

#### Without access key

```
$ firebase2graphql https://hge.herokuapp.com -d ./path/to/db.json
```

#### With access key

```
$ firebase2graphql https://hge.herokuapp.com -k <access-key> -d ./path/to/db.json
```

### Command

```bash
$ firebase2graphql URL [flags]
```

#### Args

* `URL`: The URL where Hasura GraphQL Engine is running

#### Options

- `-d --db`: path to the JS file that exports your sample JSON database
- `-o --overwrite`: Overwrite tables if they already exist in database
- `-v --version`: show CLI version
- `-h, --help`: show CLI help

---
Maintained with ♡ by <a href="https://hasura.io">Hasura</a>