const echo = (req, res) => {
  try {
    const { event: { op, data }, table: { name, schema } } = req.body;
    const response = { message: 'received event', data: { op, data, name, schema } };
    console.log('--->', response);
    return res.json(response);
  } catch (err) {
    console.error('xxx>', err);
    return res.status(500).json({ error: err.message || err });
  }
};

export default echo;
