const {json, send} = require('micro')

module.exports = async (req, res) => {
  let js
  try {
    js = await json(req)
  } catch (err) {
    send(res, 400, {'error': err.message})
  }
  let message = 'Not able to process request'
  if (js.op == 'INSERT' && js.table == 'notes') {
    message = `New note ${js.data.new.id} inserted, with data: ${js.data.new.note}`;
  } else if (js.op == 'UPDATE' && js.table == 'notes') {
    message = `note ${js.data.new.id} updated, with data: ${js.data.new.note}`;
  } else if (js.op == 'DELETE' && js.table == 'notes') {
    message = `New note ${js.data.old.id} deleted, with data: ${js.data.old.note}`;
  }
  send(res, 200, {'message': message})
};
