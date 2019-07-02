export const frequentlyUsedColumns = [
  {
    name: 'id',
    type: 'serial',
    typeText: 'integer (auto-increment)',
    primary: true,
    default: null,
    getDependentSql: null,
  },
  {
    name: 'id',
    type: 'uuid',
    typeText: 'UUID',
    primary: true,
    default: 'gen_random_uuid()',
    getDependentSql: null,
  },
  {
    name: 'created_at',
    type: 'timestamp with time zone',
    typeText: 'timestamp',
    primary: false,
    default: 'now()',
    getDependentSql: null,
  },
  {
    name: 'updated_at',
    type: 'timestamp with time zone',
    typeText: 'timestamp',
    primary: false,
    default: 'now()',
    getDependentSql: (schemaName, tableName, columnName) => {
      return `
        CREATE OR REPLACE FUNCTION "${schemaName}".set_current_timestamp()
        RETURNS TRIGGER AS $$
        DECLARE
          _new record;
        BEGIN
          _new := NEW;
          _new."${columnName}" = NOW();
          RETURN _new;
        END;
        $$ LANGUAGE plpgsql;
        CREATE TRIGGER "trigger_set_${tableName}_updated_at"
        BEFORE UPDATE ON "${schemaName}"."${tableName}"
        FOR EACH ROW
        EXECUTE PROCEDURE "${schemaName}".set_current_timestamp();
      `;
    },
  },
];

export const getFreqUsedColDisplayInfo = c => {
  const title = c.name;
  const subTitle = `${c.typeText}; ${
    c.default ? `default ${c.default};` : ''
  } ${c.primary ? 'primary key' : ''}`;
  return {
    title,
    subTitle,
  };
};
