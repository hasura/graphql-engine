## Development

#### Build Docker Image
```bash
docker build -t tutorial-site:0.1 -f Dockerfile.localdev .
```

#### Run Container in Dev Mode

```bash
docker run -ti -p 8080:8080 -v /path/to/graphql-engine/community/learn/graphql-tutorials/tutorials/ios-apollo/tutorial-site/content:/gatsby-gitbook-starter/content -v /path/to/graphql-engine/community/learn/graphql-tutorials/tutorials/ios-apollo/tutorial-site/config.js:/gatsby-gitbook-starter/config.js tutorial-site:0.1
```

Two volumes are mounted. One for `content` and one for `config.js`. This is required for hot-reloading. 

Restart docker container
- In case there are new files
- Gatsby cache needs to be updated
