export const frequentlyUsedColumns = [
  {
    name: 'id',
    type: 'serial',
    typeText: 'integer (auto-increment)',
    primary: true,
  },
  {
    name: 'id',
    type: 'uuid',
    typeText: 'UUID',
    primary: true,
    default: 'gen_random_uuid()',
  },
  {
    name: 'created_at',
    type: 'timestamptz',
    typeText: 'timestamp',
    default: 'now()',
  },
  {
    name: 'updated_at',
    type: 'timestamptz',
    typeText: 'timestamp',
    default: 'now()',
    dependentSQLGenerator: (schemaName, tableName, columnName) => {
      return `
CREATE OR REPLACE FUNCTION "${schemaName}".set_current_timestamp_${columnName}()
RETURNS TRIGGER AS $$
DECLARE
  _new record;
BEGIN
  _new := NEW;
  _new."${columnName}" = NOW();
  RETURN _new;
END;
$$ LANGUAGE plpgsql;
CREATE TRIGGER "trigger_set_${schemaName}_${tableName}_${columnName}"
BEFORE UPDATE ON "${schemaName}"."${tableName}"
FOR EACH ROW
EXECUTE PROCEDURE "${schemaName}".set_current_timestamp_${columnName}();
`;
    },
    warning:
      'This will create a before update trigger on the table to set the value of ' +
      'this column. Once the table is created, deleting this column without dropping the ' +
      'trigger will cause updates to fail for the table',
  },
];

export const getFreqUsedColDisplayInfo = c => {
  const title = c.name;
  const subTitle = `${c.typeText}; ${
    c.default ? `default: ${c.default};` : ''
  } ${c.primary ? 'primary key' : ''}`;
  return {
    title,
    subTitle,
  };
};
