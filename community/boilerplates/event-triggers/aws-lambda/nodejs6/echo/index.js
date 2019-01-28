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

    if (request.table.name === "notes" && request.event.op === "INSERT") {
        response.body = `New note ${request.event.data.new.id} inserted, with data: ${request.event.data.new.note}`;
    }
    else if (request.table.name === "notes" && request.event.op === "UPDATE") {
        response.body = `Note ${request.event.data.new.id} updated, with data: ${request.event.data.new.note}`;
    }
    else if (request.table.name === "notes" && request.event.op === "DELETE") {
        response.body = `Note ${request.event.data.old.id} deleted, with data: ${request.event.data.old.note}`;
    }

    callback(null, response);
};
