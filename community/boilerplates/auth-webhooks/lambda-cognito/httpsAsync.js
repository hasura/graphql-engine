const https = require('https');
const logger = require('myLogger.js');

function request(options) {
    return new Promise ((resolve, reject) => {
        const request = https.get(options);
        request.on("response", response => {
            var data = ''
            response.on("data", function(chunk) {
                data += chunk
            });
            response.on("end", function(){
                resolve(data);
            });
        });
        request.on("error", error => {
            logger("ERROR", error)
            resolve(error);
        });
    });
}

module.exports = {
    request
}