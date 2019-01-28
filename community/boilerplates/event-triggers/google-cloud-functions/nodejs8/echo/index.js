exports.function = (req, res) => {
  const { event: {op, data}, table: {name, schema} } = req.body;
  const response = {message: 'received event', data: {op, data, name, schema}};
  console.log(response);
  res.json(response);
};
