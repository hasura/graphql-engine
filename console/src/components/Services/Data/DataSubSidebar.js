import React from 'react';
import { connect } from 'react-redux';
import { Link } from 'react-router';
// import globals from '../../../Globals';

import LeftSubSidebar from '../../Common/Layout/LeftSubSidebar/LeftSubSidebar';

import { LISTING_SCHEMA, UPDATE_TRACKED_FUNCTIONS } from './DataActions';

const appPrefix = '/data';

const DataSubSidebar = ({
  schema,
  listingSchema,
  functionsList,
  listedFunctions,
  currentTable,
  schemaName,
  migrationMode,
  // children,
  dispatch,
  location,
  currentFunction,
  metadata,
}) => {
  const styles = require('../../Common/Layout/LeftSubSidebar/LeftSubSidebar.scss');
  const functionSymbol = require('../../Common/Layout/LeftSubSidebar/function.svg');
  const functionSymbolActive = require('../../Common/Layout/LeftSubSidebar/function_high.svg');

  if (metadata.ongoingRequest) {
    return null;
  }

  function tableSearch(e) {
    const searchTerm = e.target.value;
    // form new schema
    const matchedTables = [];
    schema.map(table => {
      if (table.table_name.indexOf(searchTerm) !== -1) {
        matchedTables.push(table);
      }
    });

    const matchedFuncs = functionsList.filter(
      f => f.function_name.indexOf(searchTerm) !== -1
    );

    // update schema with matchedTables
    dispatch({ type: LISTING_SCHEMA, updatedSchemas: matchedTables });
    dispatch({ type: UPDATE_TRACKED_FUNCTIONS, data: matchedFuncs });
  }

  const getSearchInput = () => {
    return (
      <input
        type="text"
        onChange={tableSearch.bind(this)}
        className="form-control"
        placeholder="search table/view/function"
        data-test="search-tables"
      />
    );
  };

  const getChildList = () => {
    let tableLinks = [
      <li className={styles.noChildren} key="no-tables-1">
        <i>No tables/views available</i>
      </li>,
    ];

    const tables = {};
    listingSchema.map(t => {
      tables[t.table_name] = t;
    });

    const currentLocation = location.pathname;

    if (listingSchema && listingSchema.length) {
      tableLinks = Object.keys(tables)
        .sort()
        .map((tableName, i) => {
          let activeTableClass = '';
          if (
            tableName === currentTable &&
            currentLocation.indexOf(currentTable) !== -1
          ) {
            activeTableClass = styles.activeTable;
          }
          if (tables[tableName].detail.table_type === 'BASE TABLE') {
            return (
              <li className={activeTableClass} key={i}>
                <Link
                  to={
                    appPrefix +
                    '/schema/' +
                    schemaName +
                    '/tables/' +
                    tableName +
                    '/browse'
                  }
                  data-test={tableName}
                >
                  <i
                    className={styles.tableIcon + ' fa fa-table'}
                    aria-hidden="true"
                  />
                  {tableName}
                </Link>
              </li>
            );
          }

          return (
            <li className={activeTableClass} key={i}>
              <Link
                to={
                  appPrefix +
                  '/schema/' +
                  schemaName +
                  '/views/' +
                  tableName +
                  '/browse'
                }
                data-test={tableName}
              >
                <i
                  className={styles.tableIcon + ' fa fa-table'}
                  aria-hidden="true"
                />
                <i>{tableName}</i>
              </Link>
            </li>
          );
        });
    }

    const dividerHr = [
      <li key={'fn-divider-1'}>
        <hr className={styles.tableFunctionDivider} />
      </li>,
    ];

    // If the listedFunctions is non empty
    if (listedFunctions.length > 0) {
      const functionHtml = listedFunctions.map((f, i) => (
        <li
          className={
            f.function_name === currentFunction ? styles.activeTable : ''
          }
          key={'fn ' + i}
        >
          <Link
            to={
              appPrefix +
              '/schema/' +
              schemaName +
              '/functions/' +
              f.function_name
            }
            data-test={f.function_name}
          >
            <div className={styles.display_inline + ' ' + styles.functionIcon}>
              <img
                src={
                  f.function_name === currentFunction
                    ? functionSymbolActive
                    : functionSymbol
                }
              />
            </div>
            {f.function_name}
          </Link>
        </li>
      ));

      tableLinks = [...tableLinks, ...dividerHr, ...functionHtml];
    } else if (
      functionsList.length !== listedFunctions.length &&
      listedFunctions.length === 0
    ) {
      const noFunctionResult = [
        <li className={styles.noChildren}>
          <i>No matching functions available</i>
        </li>,
      ];

      tableLinks = [...tableLinks, ...dividerHr, ...noFunctionResult];
    }

    return tableLinks;
  };

  return (
    <LeftSubSidebar
      migrationMode={migrationMode}
      searchInput={getSearchInput()}
      heading={`Tables (${schema.length})`}
      addLink={'/data/schema/' + schemaName + '/table/add'}
      addLabel={'Add Table'}
      addTestString={'sidebar-add-table'}
      childListTestString={'table-links'}
    >
      {getChildList()}
    </LeftSubSidebar>
  );
};

const mapStateToProps = state => {
  return {
    schemaName: state.tables.currentSchema,
    schema: state.tables.allSchemas,
    listingSchema: state.tables.listingSchemas,
    currentTable: state.tables.currentTable,
    migrationMode: state.main.migrationMode,
    functionsList: state.tables.trackedFunctions,
    listedFunctions: state.tables.listedFunctions,
    currentFunction: state.functions.functionName,
    serverVersion: state.main.serverVersion ? state.main.serverVersion : '',
    metadata: state.metadata,
  };
};

export default connect(mapStateToProps)(DataSubSidebar);
