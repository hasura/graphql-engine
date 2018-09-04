const {json, send} = require('micro')
const { query } = require('graphqurl');
const accessKey = process.env.ACCESS_KEY;

module.exports = async (req, res) => {
  let js
  try {
    js = await json(req)
  } catch (err) {
    send(res, 400, {'error': err.message})
  }
  let q = `
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
  `
  query(
    {
      query: q,
      endpoint: 'https://hge-events-zeit-node.herokuapp.com/v1alpha1/graphql',
      variables: {noteId: js.data.new.id, data: `${js.data.new.note} processed with event id ${js.id} with trigger name ${js.trigger_name}` },
      headers: {
        'x-access-key': accessKey,
        'Content-Type': 'application/json'
      }
    }
  ).then((response) => {
    console.log(response)
    send(res, 200, {'message': response})
  }).catch((error) => {
    send(res, 400, {'message': error})
  })
}
