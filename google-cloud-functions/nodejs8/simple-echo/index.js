exports.function = (req, res) => {
  const { op, data, table, schema } = req.body;
  const response = {message: 'received event', data: {op, data, table, schema}};
  console.log(response);
  res.json(response);
};
