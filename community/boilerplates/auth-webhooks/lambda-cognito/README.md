# Context

This is a lambda function I wrote that is behind an API gateway. The URL to invoke the lambda is passed to my Hasura instance and the POST method is specified.

This is tested and works. I also pass the encoded JWT from my clients like this

`Authorization: Bearer <JWT>`

The code has a lot of comments and links.

# Deploy

To deploy you will have to add your custom code to populate the headers. Just like the other boiler plates.

Here is a [link to deploy an NPM package to lambda](https://docs.aws.amazon.com/lambda/latest/dg/nodejs-create-deployment-pkg.html).