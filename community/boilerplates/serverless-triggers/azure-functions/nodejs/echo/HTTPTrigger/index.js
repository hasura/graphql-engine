module.exports = function (context, req) {
    context.log('JavaScript HTTP trigger function processed a request.');

    if (req.body && req.body) {
        context.res = {
            // status: 200, /* Defaults to 200 */
            body: req.body
        };
    }
    else {
        context.res = {
            status: 400,
            body: "Please pass a request body"
        };
    }
    context.done();
};