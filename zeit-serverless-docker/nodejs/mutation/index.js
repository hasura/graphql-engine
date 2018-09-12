const {json, send} = require('micro');
const { query } = require('graphqurl');

const accessKey = process.env.ACCESS_KEY;
const hgeEndpoint = process.env.HGE_ENDPOINT + '/v1alpha1/graphql';

const q = `
  mutation updateNoteRevision ($noteId: Int!, $data: String!) {
    insert_note_revisions (objects: [
      {
        note_id: $noteId,
        updated_note: $data
      }
    ]) {
      affected_rows
    }
  }
`;

module.exports = async (req, res) => {
    let js;
    try {
        js = await json(req);
    } catch (err) {
        send(res, 400, {'error': err.message});
    }
    query(
        {
            query: q,
            endpoint: hgeEndpoint,
            variables: {noteId: js.event.data.old.id, data: js.event.data.old.note},
            headers: {
                'x-hasura-access-key': accessKey,
                'Content-Type': 'application/json'
            }
        }
    ).then((response) => {
        console.log(response);
        send(res, 200, {'message': response});
    }).catch((error) => {
        send(res, 400, {'message': error});
    });
};
