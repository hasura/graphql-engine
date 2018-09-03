// Lambda which just echoes back the event data

exports.handler = (event, context, callback) => {
    let request;
    try {
        request = JSON.parse(event.body);
    } catch (e) {
        return callback(null, {statusCode: 400, body: "cannot parse hasura event"});
    }

    let response = {
        statusCode: 200,
        body: ''
    };

    if (request.table === "notes" && request.op === "INSERT") {
        response.body = `New note ${request.data.id} inserted, with data: ${request.data.note}`;
    }
    else if (request.table === "notes" && request.op === "UPDATE") {
        response.body = `Note ${request.data.id} updated, with data: ${request.data.note}`;
    }
    else if (request.table === "notes" && request.op === "DELETE") {
        response.body = `Note ${request.data.id} deleted, with data: ${request.data.note}`;
    }

    callback(null, response);
};
