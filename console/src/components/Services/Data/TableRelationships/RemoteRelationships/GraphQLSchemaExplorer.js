import React from 'react';
import styles from './SchemaExplorer.scss';
import { getSchemaTree } from '../utils';
import ExplorerItem from './ExplorerItem';
import { NoRemoteSchemaPlaceholder, LoadingSkeleton } from './Placeholders';

const SchemaExplorer = ({
  schema,
  relationship,
  handleArgChange,
  handleColumnChange,
  handleRemoteFieldChange,
  tableSchema,
  loading,
}) => {
  if (!relationship.remoteSchema) {
    return <NoRemoteSchemaPlaceholder />;
  }

  if (loading || !schema) {
    return <LoadingSkeleton />;
  }

  const schemaTree = getSchemaTree(
    relationship,
    Object.values(schema._queryType._fields)
  );

  const columns = tableSchema.columns.map(c => c.column_name).sort();

  return (
    <div className={styles.schemaExplorerContainer}>
      {schemaTree.map(f => {
        const checkboxStyle = {
          marginLeft: `${(f.nesting + (f.argNesting || 0)) * 20}px`,
          color: f.isArg ? '#8B2BB9' : 'rgb(31, 97, 160)',
          fontStyle: f.isArg ? 'italic' : 'normal',
        };

        const toggle = () => {
          const checked = !f.isChecked;
          if (f.isArg) {
            handleArgChange(
              f.parentFieldName,
              f.parentFieldNesting,
              f.name,
              f.argNesting,
              checked,
              f.parentArg
            );
          } else {
            handleRemoteFieldChange(f.name, f.nesting, checked);
          }
        };

        return (
          <ExplorerItem
            label={f.name}
            checkboxStyle={checkboxStyle}
            toggle={toggle}
            item={f}
            columns={columns}
            handleColumnChange={handleColumnChange}
            key={`${f.name}-${f.nesting}-${f.argNesting || ''}-${
              f.isArg && f.parentArg ? f.parentArg : ''
            }`}
          />
        );
      })}
    </div>
  );
};

export default SchemaExplorer;
