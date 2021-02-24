import React, { useEffect, useState } from 'react';
import { connect } from 'react-redux';
import LeftSubSidebar from '../../Common/Layout/LeftSubSidebar/LeftSubSidebar';
import { manageDatabasesRoute } from '../../Common/utils/routesUtils';
import TreeView from './TreeView';
import { getDatabaseTableTypeInfo } from './DataActions';
import { isInconsistentSource } from './utils';

const groupByKey = (list, key) =>
  list.reduce(
    (hash, obj) => ({
      ...hash,
      [obj[key]]: (hash[obj[key]] || []).concat(obj),
    }),
    {}
  );

const DataSubSidebar = props => {
  const {
    migrationMode,
    dispatch,
    onDatabaseChange,
    onSchemaChange,
    tables,
    functions,
    sources,
    currentDataSource,
    schemaList,
    currentSchema,
    enums,
    inconsistentObjects,
  } = props;

  const getItems = (schemaInfo = null) => {
    let sourceItems = [];
    sources.forEach(source => {
      if (isInconsistentSource(source.name, inconsistentObjects)) return;

      const sourceItem = { name: source.name, type: 'database' };
      const sourceTables = !source.tables
        ? []
        : source.tables.map(data => {
            const is_enum = data.is_enum ? true : false;
            return {
              name: data.table.name,
              schema: data.table.schema,
              type: 'table',
              is_enum: is_enum,
            };
          });
      const sourceFunctions = !source.functions
        ? []
        : source.functions.map(data => ({
            name: data.function.name,
            schema: data.function.schema,
            type: 'function',
          }));

      const schemaGroups = groupByKey(
        [...sourceTables, ...sourceFunctions],
        'schema'
      );

      // Find out the difference between schemas from metadata and SchemaList from state
      const schemasFromMetadata = Array.from(
        new Set([
          ...sourceTables.map(i => i.schema),
          ...sourceFunctions.map(i => i.schema),
        ])
      );
      const missingSchemas = schemaList.filter(
        x => !schemasFromMetadata.includes(x)
      );

      let schemaItems = [];
      Object.keys(schemaGroups).forEach(schema => {
        const schemaItem = { name: schema, type: 'schema' };
        const tableItems = [];
        schemaGroups[schema].forEach(table => {
          const is_view =
            schemaInfo &&
            schemaInfo[source.name][schema] &&
            schemaInfo[source.name][schema][table.name]
              ? schemaInfo[source.name][schema][table.name].table_type ===
                  'view' ||
                schemaInfo[source.name][schema][table.name].table_type ===
                  'materialized_view'
              : false;
          let type = table.type;
          if (is_view) type = 'view';
          if (table.is_enum) type = 'enum';
          tableItems.push({
            name: table.name,
            type: type,
          });
        });
        schemaItem.children = tableItems;
        schemaItems = [...schemaItems, schemaItem];
      });

      sourceItem.children = schemaItems;

      if (source.name === currentDataSource) {
        sourceItem.children = [
          ...missingSchemas.map(schemaName => ({
            name: schemaName,
            type: 'schema',
            children: [],
          })),
          ...sourceItem.children,
        ];
      }

      sourceItems = [...sourceItems, sourceItem];
    });
    return sourceItems;
  };

  const [treeViewItems, setTreeViewItems] = useState([]);

  const updateTreeViewItemsWithSchemaInfo = () => {
    const schemaPromises = [];
    sources.forEach(source => {
      const currentSourceTables = sources
        .filter(i => i.name === source.name)[0]
        .tables.map(i => `'${i.table.name}'`);
      schemaPromises.push(
        dispatch(
          getDatabaseTableTypeInfo(
            source.kind,
            source.name,
            currentSourceTables
          )
        ).then(data => ({ source: source.name, schemaInfo: data }))
      );
    });
    Promise.all(schemaPromises).then(data => {
      const schemaInfo = {};
      data.forEach(item => {
        schemaInfo[item.source] = item.schemaInfo;
      });
      const newItems = getItems(schemaInfo);
      setTreeViewItems(newItems);
    });
  };

  useEffect(() => {
    updateTreeViewItemsWithSchemaInfo();
  }, [sources.length, tables, functions, enums, schemaList]);

  const databasesCount = treeViewItems?.length || 0;

  return (
    <LeftSubSidebar
      showAddBtn={migrationMode}
      heading={`Databases (${databasesCount})`}
      addLink={manageDatabasesRoute}
      addLabel={'Manage'}
      addTestString={'sidebar-manage-database'}
      childListTestString={'table-links'}
    >
      <TreeView
        items={treeViewItems}
        onDatabaseChange={onDatabaseChange}
        onSchemaChange={onSchemaChange}
        currentDataSource={currentDataSource}
        currentSchema={currentSchema}
      />
    </LeftSubSidebar>
  );
};

const mapStateToProps = state => {
  return {
    migrationMode: state.main.migrationMode,
    sources: state.metadata.metadataObject.sources,
    inconsistentObjects: state.metadata.inconsistentObjects,
    tables: state.metadata.metadataObject.sources.map(s => s.tables).flat()
      .length,
    enums: state.metadata.metadataObject.sources
      .map(s => s.tables)
      .flat()
      .filter(item => item.hasOwnProperty('is_enum')).length,
    functions: state.metadata.metadataObject.sources
      .map(s => s.functions)
      .flat().length,
    currentDataSource: state.tables.currentDataSource,
    currentSchema: state.tables.currentSchema,
    schemaList: state.tables.schemaList,
  };
};

export default connect(mapStateToProps)(DataSubSidebar);
