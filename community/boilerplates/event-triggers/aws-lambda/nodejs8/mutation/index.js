// Lambda which gets triggered on insert, and in turns performs a mutation

const fetch = require('node-fetch');

const accessKey = process.env.ACCESS_KEY;
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

exports.handler = async (event) => {
  try {
    const qv = { noteId: event.body.event.data.old.id, data: event.body.event.data.old.note };
    const result = await fetch(hgeEndpoint + '/v1/graphql', {
      method: 'POST',
      body: JSON.stringify({ query: query, variables: qv }),
      headers: { 'Content-Type': 'application/json', 'x-hasura-access-key': accessKey },
    });
    
    const { errors, data } = await result.json();

    if (errors) {
      throw new Error(errors);
    } else {
      return {
        statusCode: 200,
        body: "success"
      };
    }
  } catch (e) {
    return {
      statusCode: 400,
      body: "cannot parse hasura event"
    };
  }
};
