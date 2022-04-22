export const tables = {
  result_type: 'TuplesOk',
  result: [
    ['tables'],
    [
      '[{"table_schema":"public","table_name":"user","table_type":"TABLE","comment":null,"columns":[{"comment": null, "data_type": "integer", "table_name": "user", "column_name": "id", "is_nullable": "NO", "table_schema": "public", "column_default": "nextval(\'user_id_seq\'::regclass)", "data_type_name": "int4", "ordinal_position": 1}, {"comment": null, "data_type": "text", "table_name": "user", "column_name": "description", "is_nullable": "NO", "table_schema": "public", "column_default": null, "data_type_name": "text", "ordinal_position": 3}, {"comment": null, "data_type": "text", "table_name": "user", "column_name": "name", "is_nullable": "NO", "table_schema": "public", "column_default": null, "data_type_name": "text", "ordinal_position": 2}],"triggers":[],"view_info":null}, {"table_schema":"public","table_name":"stuff","table_type":"TABLE","comment":null,"columns":[{"comment": null, "data_type": "integer", "table_name": "stuff", "column_name": "id", "is_nullable": "NO", "table_schema": "public", "column_default": "nextval(\'stuff_id_seq\'::regclass)", "data_type_name": "int4", "ordinal_position": 1}, {"comment": null, "data_type": "text", "table_name": "stuff", "column_name": "name", "is_nullable": "NO", "table_schema": "public", "column_default": null, "data_type_name": "text", "ordinal_position": 2}],"triggers":[],"view_info":null}, {"table_schema":"schema2","table_name":"things","table_type":"TABLE","comment":null,"columns":[{"comment": null, "data_type": "integer", "table_name": "things", "column_name": "id", "is_nullable": "NO", "table_schema": "schema2", "column_default": "nextval(\'schema2.things_id_seq\'::regclass)", "data_type_name": "int4", "ordinal_position": 1}, {"comment": null, "data_type": "text", "table_name": "things", "column_name": "description", "is_nullable": "NO", "table_schema": "schema2", "column_default": null, "data_type_name": "text", "ordinal_position": 3}, {"comment": null, "data_type": "text", "table_name": "things", "column_name": "name", "is_nullable": "NO", "table_schema": "schema2", "column_default": null, "data_type_name": "text", "ordinal_position": 2}],"triggers":[],"view_info":null}]',
    ],
  ],
};

export const albumTableColumnsResult = {
  result_type: 'TuplesOk',
  result: [
    ['database', 'table_schema', 'table_name', 'column_name', 'data_type'],
    ['chinook', 'public', 'Album', 'AlbumId', 'integer'],
    ['chinook', 'public', 'Album', 'Title', 'character varying'],
    ['chinook', 'public', 'Album', 'ArtistId', 'integer'],
  ],
};

export const artistTableColumnsResult = {
  result_type: 'TuplesOk',
  result: [
    ['database', 'table_schema', 'table_name', 'column_name', 'data_type'],
    ['chinook', 'public', 'Artist', 'Id', 'integer'],
    ['chinook', 'public', 'Artist', 'Name', 'character varying'],
    ['chinook', 'public', 'Artist', 'Age', 'integer'],
  ],
};

export const userInfoTableColumnsResult = {
  result_type: 'TuplesOk',
  result: [
    ['database', 'table_schema', 'table_name', 'column_name', 'data_type'],
    ['chinook', 'user', 'userInfo', 'Id', 'integer'],
    ['chinook', 'user', 'userInfo', 'FirstName', 'character varying'],
    ['chinook', 'user', 'userInfo', 'LastName', 'character varying'],
    ['chinook', 'user', 'userInfo', 'Age', 'integer'],
  ],
};

export const userAddressTableColumnsResult = {
  result_type: 'TuplesOk',
  result: [
    ['database', 'table_schema', 'table_name', 'column_name', 'data_type'],
    ['chinook', 'user', 'userAddress', 'Id', 'integer'],
    ['chinook', 'user', 'userAddress', 'Block', 'character varying'],
    ['chinook', 'user', 'userAddress', 'Street', 'character varying'],
    ['chinook', 'user', 'userAddress', 'City', 'character varying'],
    ['chinook', 'user', 'userAddress', 'Country', 'character varying'],
    ['chinook', 'user', 'userAddress', 'CountryCode', 'integer'],
  ],
};
