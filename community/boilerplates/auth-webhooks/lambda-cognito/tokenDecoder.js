/*

This code is taken from an AWS sample code repository. The code can be found here:

https://github.com/awslabs/aws-support-tools/blob/master/Cognito/decode-verify-jwt/decode-verify-jwt.js

It's seperated out into is own file and function for two reason.

First, It's easier to read and possibly test.

Second, Ther is a network request here to go download the secret keys from Cognito. According to lambda best practices here:

https://docs.aws.amazon.com/lambda/latest/dg/best-practices.html

It's good practice to seperate out logic from the app entry point. This can make the function run faster especially when calls happen fast which I can imagine will be the case with this web hook.

*/

//Required to complete the AWS Code example
var jose = require('node-jose');

//In the lambda console you can define environment variables. https://docs.aws.amazon.com/lambda/latest/dg/env_variables.html
const region = process.env.AWS_COGNITO_REGION;
const cognitoPool = process.env.AWS_COGNITO_USER_POOL;

/*
Here we are using the URL that the AWS Documentation says you can get your JSON Web Key Set (JWKS)
https://docs.aws.amazon.com/cognito/latest/developerguide/amazon-cognito-user-pools-using-tokens-verifying-a-jwt.html
*/
const keys_url = `https://cognito-idp.${region}.amazonaws.com/${cognitoPool}/.well-known/jwks.json`;
const asyncRequest = require('httpsAsync.js');


async function decode(token) {
    /*
    This is all from the sample AWS Code layed out in the first link above. The only difference is that I made an async/ await compatable get request for the token. I also provide 
    but commented out a modified sample to compare the audience key in the token to multiple application client secrets. Application client secrets are made from the cognito console. 
    The thoght is that there could be multiple client, each with their own keys, using Hasura.
    */

    //1) Parse out and decode the Key ID (kid) from the incomming token.
    const sections = token.split(".");
    var header = jose.util.base64url.decode(sections[0]);
    header = JSON.parse(header);
    const kid = header.kid;

    //2) Go request the JSON Web Key Set (JWKS) from the constructed URL
    const secretJSON = await asyncRequest.request(keys_url);
    const keys = JSON.parse(secretJSON)['keys'];

    //3) Search for the kid in the downloaded public keys that matches the kid from the request.
    var key_index = -1;
    for (var i=0; i < keys.length; i++) {
            if (kid == keys[i].kid) {
                key_index = i;
                break;
            }
    }
    if (key_index == -1) {
        throw "Public key not found in jwks.json";
    };

    //4) Take the key and decode the claims!
    const myKey = keys[key_index];
    const publicKey = await jose.JWK.asKey(myKey).then();
    const verifiedToken = await jose.JWS.createVerify(publicKey).verify(token).then();
    const claims = JSON.parse(verifiedToken.payload);

    // //5) Additional actions for tighter security.
    // // Verify the token expiration
    // var current_ts = Math.floor(new Date() / 1000);
    // if (current_ts > claims.exp) {
    //     throw "Expired Token";
    // };

    // //Verify claims aginst known client ID's in Cognito
    // const arrayOfCognitoClientIDS = ['id1', 'id2']
    // if (contains(arrayOfCognitoClientIDS, claims.aud) == false) {
    //     throw "Now for these eyes!"
    // }
    return claims;
};

//https://stackoverflow.com/questions/237104/how-do-i-check-if-an-array-includes-an-object-in-javascript?page=1&tab=votes#tab-top
function contains(a, obj) {
    var i = a.length;
    while (i--) {
       if (a[i] === obj) {
           return true;
       }
    }
    return false;
}

//Make this useful.
module.exports = {
    decode
}