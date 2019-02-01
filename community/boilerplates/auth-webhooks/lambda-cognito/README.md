# Context

This is a lambda function I wrote that is behind an API gateway. The URL to invoke the lambda is passed to my Hasura instance and the POST method is specified.

This is tested and works. I also pass the encoded JWT from my clients like this

`Authorization: Bearer <JWT>`

The code has a lot of comments and links.