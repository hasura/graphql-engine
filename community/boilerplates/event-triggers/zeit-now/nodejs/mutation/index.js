const {json, send} = require('micro');
const { query } = require('graphqurl');

const HGE_ENDPOINT = process.env.HGE_ENDPOINT;

const MUTATION_UPDATE_NOTE_REVISION = `
  mutation updateNoteRevision ($object: note_revision_insert_input!) {
    insert_note_revision (objects: [$object]) {
      affected_rows
      returning {
        id
      }
    }
  }
`;

module.exports = async (req, res) => {
  let payload;
  try {
    payload = await json(req);
  } catch (error) {
    send(res, 400, { error });
    return;
  }

  const { id, event: {op, data}, table, trigger } = payload;

  try {
    const result = await query({
      query: MUTATION_UPDATE_NOTE_REVISION,
      endpoint: HGE_ENDPOINT,
      variables: {
        object: {
          note_id: data.old.id, note: data.new.note
        }
      },
    });
    send(res, 200, { result });
  } catch (error) {
    send(res, 500, { error });
  }
};
