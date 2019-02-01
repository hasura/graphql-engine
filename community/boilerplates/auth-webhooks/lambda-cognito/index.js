const logger = require('myLogger.js');
const tokenExtractor = require('tokenExtractor.js');
const tokenDecoder = require('tokenDecoder.js');

exports.handler = async (event, context, callback) => {
    //1) Get the body of the event and parse it.
    const jsonBody = JSON.parse(event.body);
    /*2) Get the token. Hasura encodes all forwarded headers in a key called "headers". We passed the encoded JWT from our application in this form.
    Authorization: Bearer <JWT>
    */
    const token = jsonBody.headers.Authorization;
    
    //3) Since our token start with "Bearer " clean that out.
    const cleanedToken = tokenExtractor.cleanOutBearer(token);

    logger("Cleaned Token", cleanedToken);
    //4) Decode the token so we have the claims in a dictionary.
    const claims = await tokenDecoder.decode(cleanedToken);

    /*
    At this point the flow becomes kind of custom. I'm expecting an "email" claim in my Token as I force users of my app to sign up with an email.

    I also save a user record in my PostgreSQL DB on the Cognito Pre Sign Up trigger

    https://docs.aws.amazon.com/cognito/latest/developerguide/user-pool-lambda-pre-sign-up.html

    I save the email as a unique key on the user record. I used this blog post to account for some edge cases

    https://hashrocket.com/blog/posts/working-with-email-addresses-in-postgresql

    This means that I can use the email parsed out of the claims to look up my user in my DB and then create headers based on my query('s).

    Another option would be to use the Pre Sign Up trigger to write the user to your DB and then save the new users 'id' as a custom attribute in Cognito. I think Cognito forwards custom attributes in the claim.

    */


    //5) Get the email
    const email = claims.email;

    //6) Do what ever you need to, to get the users roles so that you can send them back as headers to Hasura.

    logger("Decoded Email", email);
     
     const body = {
       'X-Hasura-Role': 'user',  // result.role
       'X-Hasura-User-Email': email    // result.user_id
     };
     
     const response = {
         statusCode: 200,
         body: JSON.stringify(body),
     };
     return response;
};