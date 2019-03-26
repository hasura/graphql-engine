Scripts that run to keep changing the data in the database.

```
ADMIN_SECRET=<secret> node index.js
```

```
docker build -t g2c-stocks-script:v1 .
docker run -e ADMIN_SECRET=<secret> g2c-stocks-script:v1
```
