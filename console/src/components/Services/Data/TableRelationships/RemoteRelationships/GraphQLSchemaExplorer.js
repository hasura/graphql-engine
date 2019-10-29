import React from 'react';
import styles from './SchemaExplorer.scss';
import { connect } from 'react-redux';
import { getSchemaTree } from '../utils';
import ExplorerItem from './ExplorerItem';
import { NoRemoteSchemaPlaceholder, LoadingSkeleton } from './Placeholders';
import { useIntrospectionSchema } from '../../../RemoteSchema/graphqlUtils';

const SchemaExplorer = ({
  relationship,
  handleArgChange,
  handleArgValueChange,
  handleRemoteFieldChange,
  tableSchema,
  headers: adminHeaders,
}) => {
  // introspect selected remote schema

  const { schema, loading, error, introspect } = useIntrospectionSchema(
    relationship.remoteSchema,
    adminHeaders
  );

  // When remote schema is not selected
  if (!relationship.remoteSchema) {
    return <NoRemoteSchemaPlaceholder />;
  }

  // While introspecting remote schema or when introspection fails
  if (loading) {
    return <LoadingSkeleton />;
  }

  if (error) {
    return (
      <div>
        Error introspecting remote schema.{' '}
        <a onClick={introspect}> Try again </a>
      </div>
    );
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

const mapStateToProps = state => {
  return {
    headers: state.tables.dataHeaders,
  };
};

export default connect(mapStateToProps)(SchemaExplorer);
