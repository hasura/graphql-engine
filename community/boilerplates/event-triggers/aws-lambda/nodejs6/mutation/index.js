// Lambda which gets triggered on insert, and in turns performs a mutation

const fetch = require('node-fetch');

const adminSecret = process.env.ADMIN_SECRET;
const hgeEndpoint = process.env.HGE_ENDPOINT;

const query = `
mutation updateNoteRevision ($noteId: Int!, $data: String!) {
  insert_note_revision (objects: [
    {
      note_id: $noteId,
      note: $data
    }
  ]) {
    affected_rows
  }
}
`;

exports.handler = (event, context, callback) => {
    let request;
    try {
        request = JSON.parse(event.body);
    } catch (e) {
        return callback(null, {statusCode: 400, body: "cannot parse hasura event"});
    }

    const response = {
        statusCode: 200,
        body: "success"
    };
    const qv = {noteId: request.event.data.old.id, data: request.event.data.old.note};
    fetch(hgeEndpoint + '/v1/graphql', {
        method: 'POST',
        body: JSON.stringify({query: query, variables: qv}),
        headers: {'Content-Type': 'application/json', 'x-hasura-admin-secret': adminSecret},
    })
        .then(res => res.json())
        .then(json => {
            console.log(json);
            callback(null, response);
        });
};
