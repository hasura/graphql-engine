import React from 'react';
import { connect } from 'react-redux';
import { Link } from 'react-router';

import LeftSubSidebar from '../../Common/Layout/LeftSubSidebar/LeftSubSidebar';
import GqlCompatibilityWarning from '../../Common/GqlCompatibilityWarning/GqlCompatibilityWarning';
import {
  displayTableName,
  getFunctionName,
  getSchemaTables,
  getTableName,
  checkIfTable,
  getFunctionSchema,
} from '../../Common/utils/pgUtils';
import {
  getFunctionModifyRoute,
  getSchemaAddTableRoute,
  getTableBrowseRoute,
} from '../../Common/utils/routesUtils';

class DataSubSidebar extends React.Component {
  constructor() {
    super();

    this.state = {
      searchInput: '',
    };

    this.tableSearch = this.tableSearch.bind(this);
  }

  shouldComponentUpdate(nextProps) {
    if (nextProps.metadata.ongoingRequest) {
      return false;
    }

    return true;
  }

  tableSearch(e) {
    const searchTerm = e.target.value;

    this.setState({
      searchInput: searchTerm,
    });
  }

  render() {
    const styles = require('../../Common/Layout/LeftSubSidebar/LeftSubSidebar.scss');
    const functionSymbol = require('../../Common/Layout/LeftSubSidebar/function.svg');
    const functionSymbolActive = require('../../Common/Layout/LeftSubSidebar/function_active.svg');
    const {
      currentTable,
      currentSchema,
      migrationMode,
      location,
      currentFunction,
      trackedFunctions,
      allSchemas,
    } = this.props;

    const { searchInput } = this.state;

    const trackedTablesInSchema = getSchemaTables(
      allSchemas,
      currentSchema
    ).filter(table => table.is_table_tracked);

    const filteredTableList = trackedTablesInSchema.filter(t =>
      getTableName(t).includes(searchInput)
    );

    const filteredFunctionsList = trackedFunctions.filter(f =>
      getFunctionName(f).includes(searchInput)
    );

    const getSearchInput = () => {
      return (
        <input
          type="text"
          onChange={this.tableSearch}
          className="form-control"
          placeholder={'search table/view/function'}
          data-test="search-tables"
        />
      );
    };

    const getChildList = () => {
      let childList;

      const currentLocation = location.pathname;

      let tableLinks = [];
      if (filteredTableList && filteredTableList.length) {
        const filteredTablesObject = {};
        filteredTableList.forEach(t => {
          filteredTablesObject[getTableName(t)] = t;
        });

        const sortedTableNames = Object.keys(filteredTablesObject).sort();

        tableLinks = sortedTableNames.map((tableName, i) => {
          const table = filteredTablesObject[tableName];

          const isActive =
            tableName === currentTable && currentLocation.includes(tableName);

          const iconStyle = table.is_enum ? 'fa-list-ul' : 'fa-table';

          return (
            <li
              className={isActive ? styles.activeLink : ''}
              key={'table ' + i}
            >
              <Link
                to={getTableBrowseRoute(
                  currentSchema,
                  tableName,
                  checkIfTable(table)
                )}
                data-test={tableName}
              >
                <i
                  className={`${styles.tableIcon} fa ${iconStyle}`}
                  aria-hidden="true"
                />
                {displayTableName(table)}
              </Link>
              <GqlCompatibilityWarning
                identifier={tableName}
                className={styles.add_mar_left_mid}
              />
            </li>
          );
        });
      }

      let functionLinks = [];
      if (filteredFunctionsList && filteredFunctionsList.length > 0) {
        const filteredFunctionsObject = {};
        filteredFunctionsList.forEach(f => {
          filteredFunctionsObject[getFunctionName(f)] = f;
        });

        const sortedFunctionNames = Object.keys(filteredFunctionsObject).sort();

        functionLinks = sortedFunctionNames.map((funcName, i) => {
          const func = filteredFunctionsObject[funcName];

          const isActive =
            funcName === currentFunction && currentLocation.includes(funcName);

          return (
            <li className={isActive ? styles.activeLink : ''} key={'fn ' + i}>
              <Link
                to={getFunctionModifyRoute(getFunctionSchema(func), funcName)}
                data-test={funcName}
              >
                <img
                  src={isActive ? functionSymbolActive : functionSymbol}
                  className={styles.functionIcon}
                />
                <span>{funcName}</span>
              </Link>
            </li>
          );
        });
      }

      childList = [...tableLinks, ...functionLinks];

      if (childList.length === 0) {
        childList = [
          <li className={styles.noChildren} key="no-tables-1">
            <i>No tables/views/functions available</i>
          </li>,
        ];
      }

      return childList;
    };

    const tablesViewsFunctionsCount =
      filteredTableList.length + filteredFunctionsList.length;

    return (
      <LeftSubSidebar
        showAddBtn={migrationMode}
        searchInput={getSearchInput()}
        heading={`Tables/Views/Functions (${tablesViewsFunctionsCount})`}
        addLink={getSchemaAddTableRoute(currentSchema)}
        addLabel={'Add Table'}
        addTestString={'sidebar-add-table'}
        childListTestString={'table-links'}
      >
        {getChildList()}
      </LeftSubSidebar>
    );
  }
}

const mapStateToProps = state => {
  return {
    migrationMode: state.main.migrationMode,
    trackedFunctions: state.tables.trackedFunctions,
    currentFunction: state.functions.functionName,
    allSchemas: state.tables.allSchemas,
    currentTable: state.tables.currentTable,
    currentSchema: state.tables.currentSchema,
    serverVersion: state.main.serverVersion ? state.main.serverVersion : '',
    metadata: state.metadata,
  };
};

export default connect(mapStateToProps)(DataSubSidebar);
