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
    console.log(request);

    if (request.table === "notes" && request.op === "INSERT") {
        response.body = `New note ${request.data.new.id} inserted, with data: ${request.data.new.note}`;
    }
    else if (request.table === "notes" && request.op === "UPDATE") {
        response.body = `Note ${request.data.new.id} updated, with data: ${request.data.new.note}`;
    }
    else if (request.table === "notes" && request.op === "DELETE") {
        response.body = `Note ${request.data.old.id} deleted, with data: ${request.data.old.note}`;
    }

    callback(null, response);
};
