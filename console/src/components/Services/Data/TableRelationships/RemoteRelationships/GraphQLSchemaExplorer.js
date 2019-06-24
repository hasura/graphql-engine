import React, { useState, useEffect } from 'react';
import { buildClientSchema } from 'graphql';
import styles from './SchemaExplorer.scss';
import { getSchemaTree } from '../utils';
import ExplorerItem from './ExplorerItem';
import { NoRemoteSchemaPlaceholder, LoadingSkeleton } from './Placeholders';
import { introspectRemoteSchema } from '../Actions';

const SchemaExplorer = ({
  relationship,
  handleArgChange,
  handleArgValueChange,
  handleRemoteFieldChange,
  tableSchema,
  loading,
  dispatch,
}) => {
  // introspect selected remote schema
  const [schema, setRemoteSchema] = useState(null);
  useEffect(() => {
    if (relationship.remoteSchema) {
      const introspectionCallback = introspectionResult => {
        const clientSchema = buildClientSchema(introspectionResult.data);
        setRemoteSchema(clientSchema);
      };
      dispatch(
        introspectRemoteSchema(relationship.remoteSchema, introspectionCallback)
      );
    }
    return () => {
      setRemoteSchema(null);
    };
  }, [relationship.remoteSchema]);

  // When remote schema is not selected
  if (!relationship.remoteSchema) {
    return <NoRemoteSchemaPlaceholder />;
  }

  // While introspecting remote schema or when introspection fails
  if (loading || !schema) {
    return <LoadingSkeleton />;
  }

  // generate a list of selected/unselected checkboxes from the graphql schema and the relationship
  const schemaTree = getSchemaTree(
    relationship,
    Object.values(schema._queryType._fields)
  );

  // table columns
  const columns = tableSchema.columns.map(c => c.column_name).sort();

  return (
    <div className={styles.schemaExplorerContainer}>
      {schemaTree.map(f => {
        // inline styles
        const checkboxStyle = {
          marginLeft: `${(f.nesting + (f.argNesting || 0)) * 20}px`,
          color: f.isArg ? '#8B2BB9' : 'rgb(31, 97, 160)',
          fontStyle: f.isArg ? 'italic' : 'normal',
        };

        // toggle checkbox
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
            handleArgValueChange={handleArgValueChange}
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
