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
    defaultText: 'now() + trigger to set value on update',
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
COMMENT ON TRIGGER "trigger_set_${schemaName}_${tableName}_${columnName}" ON "${schemaName}"."${tableName}" 
IS 'trigger to set value of column "${columnName}" to current timestamp on row update';
`;
    },
  },
];

export const getFreqUsedColDisplayInfo = c => {
  const title = c.name;

  const typeText = c.typeText + '; ';
  const defaultText =
    c.defaultText || c.default
      ? `default: ${c.defaultText || c.default}; `
      : '';
  const pkText = c.primary ? 'primary key; ' : '';

  const subTitle = typeText + defaultText + pkText;

  return {
    title,
    subTitle,
  };
};
