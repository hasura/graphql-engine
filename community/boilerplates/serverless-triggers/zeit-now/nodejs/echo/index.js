const { json, send } = require('micro');

module.exports = async (req, res) => {
  let payload;
  try {
    payload = await json(req);
  } catch (error) {
    send(res, 400, { error });
    return;
  }

  const { id, event: {op, data}, table, trigger } = payload;

  send(res, 200, {
    message: `received '${id}' for '${op}' operation on '${table.name}' table in '${table.schema}' schema from '${trigger.name}' trigger`,
    oldData: data.old,
    newData: data.new,
  });
};
