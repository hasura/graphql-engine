import React from 'react';
import Dropdown from '../../../../Common/Dropdown/Dropdown';
import Button from '../../../../Common/Button/Button';

const frequentlyUsedColumns = [
  {
    name: 'id',
    validFor: ['add'],
    type: 'serial',
    typeText: 'integer (auto-increment)',
    primary: true,
  },
  {
    name: 'id',
    validFor: ['add'],
    type: 'uuid',
    typeText: 'UUID',
    primary: true,
    default: 'gen_random_uuid()',
  },
  {
    name: 'created_at',
    validFor: ['add', 'modify'],
    type: 'timestamptz',
    typeText: 'timestamp',
    default: 'now()',
  },
  {
    name: 'updated_at',
    validFor: ['add', 'modify'],
    type: 'timestamptz',
    typeText: 'timestamp',
    default: 'now()',
    defaultText: 'now() + trigger to set value on update',
    dependentSQLGenerator: (schemaName, tableName, columnName) => {
      const upSql = `
CREATE OR REPLACE FUNCTION "${schemaName}"."set_current_timestamp_${columnName}"()
RETURNS TRIGGER AS $$
DECLARE
  _new record;
BEGIN
  _new := NEW;
  _new."${columnName}" = NOW();
  RETURN _new;
END;
$$ LANGUAGE plpgsql;
CREATE TRIGGER "set_${schemaName}_${tableName}_${columnName}"
BEFORE UPDATE ON "${schemaName}"."${tableName}"
FOR EACH ROW
EXECUTE PROCEDURE "${schemaName}"."set_current_timestamp_${columnName}"();
COMMENT ON TRIGGER "set_${schemaName}_${tableName}_${columnName}" ON "${schemaName}"."${tableName}" 
IS 'trigger to set value of column "${columnName}" to current timestamp on row update';
`;

      const downSql = `DROP TRIGGER IF EXISTS "set_${schemaName}_${tableName}_${columnName}" ON "${schemaName}"."${tableName}";`;

      return {
        upSql,
        downSql,
      };
    },
  },
];

const getFreqUsedColDisplayInfo = c => {
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

const FrequentlyUsedColumnSelector = ({
  onSelect,
  action = null,
  dispatch = null,
}) => {
  const frequentlyUsedColumnsOptions = () => {
    return frequentlyUsedColumns
      .filter(fuc => !action || fuc.validFor.includes(action))
      .map(fuc => {
        const { title, subTitle } = getFreqUsedColDisplayInfo(fuc);
        return {
          content: (
            <div>
              <div>
                <b>{title}</b>
              </div>
              <div>{subTitle}</div>
            </div>
          ),
          onClick: () => (dispatch ? dispatch(onSelect(fuc)) : onSelect(fuc)),
        };
      });
  };

  return (
    <Dropdown
      testId={'frequently-used-columns'}
      options={frequentlyUsedColumnsOptions()}
      position="bottom"
      key={'frequently-used-columns'}
      keyPrefix={'frequently-used-columns'}
    >
      <Button color="white" size="xs">
        + Frequently used columns
      </Button>
    </Dropdown>
  );
};

export default FrequentlyUsedColumnSelector;
