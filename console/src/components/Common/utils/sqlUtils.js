import sqlFormatter from 'sql-formatter';

export const formatSql = sql => {
  return sqlFormatter.format(sql); // TODO: failing for plpgsql
};

export const formatRequest = request => {
  const formattedRequest = { ...request, args: { ...request.args } };

  if (request.type === 'run_sql' && formattedRequest.args.sql) {
    formattedRequest.args.sql = formatSql(request.args.sql);
  }

  return formattedRequest;
};

export const sqlEscapeText = text => {
  let _text = text;

  if (_text) {
    _text = _text.replace(/'/g, "\\'");
  }

  return `E'${_text}'`;
};
