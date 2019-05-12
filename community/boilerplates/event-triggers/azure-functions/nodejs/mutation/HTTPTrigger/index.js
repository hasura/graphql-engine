const { query } = require('graphqurl');
const ADMIN_SECRET = process.env.ADMIN_SECRET;
const HGE_ENDPOINT = process.env.HGE_ENDPOINT;

const MUTATION_NOTE_REVISION = `
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

module.exports = function (context, req) {
    context.log('JavaScript HTTP trigger function processed a request.');
    try {
        context.log('Inside');
        const { event: {op, data}, table } = req.body;
        context.log(data);
        context.log(data.new.id);
        const qv = {noteId: data.old.id, data: data.old.note};
        const hgeResponse = query({
              query: MUTATION_NOTE_REVISION,
              endpoint: HGE_ENDPOINT + '/v1/graphql',
              variables: qv,
              headers: {
                'x-hasura-admin-secret': ADMIN_SECRET
              }
        }).then((response) => {
            context.log(response);
            context.log('After query');
            context.res = {
                body: {
                    error: false,
                    data: response
                }
            };
            context.done();
        }).catch((error) => {
            console.error(JSON.stringify(error));
            context.res = {
                status: 500,
                body: {
                    error: true,
                    data: JSON.stringify(error)
                }
            };
            context.done();
        });
    } catch(e) {
        context.res = {
            status: 400,
            body: "An error occured."
        };
        context.done();
    }
};
