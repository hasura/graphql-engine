import React from 'react';
import { connect } from 'react-redux';
import { Link } from 'react-router';

import LeftSubSidebar from '../../Common/Layout/LeftSubSidebar/LeftSubSidebar';
import gqlPattern from './Common/GraphQLValidation';
import GqlCompatibilityWarning from '../../Common/GqlCompatibilityWarning/GqlCompatibilityWarning';
import {
  displayTableName,
  getFunctionName,
  getTableName,
} from '../../Common/utils/pgUtils';
import {
  getFunctionModifyRoute,
  getSchemaAddTableRoute,
  getTableBrowseRoute,
} from '../../Common/utils/routesUtils';

class DataSubSidebar extends React.Component {
  constructor() {
    super();

    this.tableSearch = this.tableSearch.bind(this);
    this.state = {
      trackedTables: [],
      searchInput: '',
    };
  }

  static getDerivedStateFromProps(props) {
    const { currentSchema, schema } = props;

    const trackedTables = schema.filter(
      table => table.is_table_tracked && table.table_schema === currentSchema
    );

    return {
      trackedTables: trackedTables,
    };
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
      functionsList,
      currentTable,
      currentSchema,
      migrationMode,
      location,
      currentFunction,
    } = this.props;

    const { trackedTables, searchInput } = this.state;

    const trackedTablesLength = trackedTables.length;

    const tableList = trackedTables.filter(t =>
      getTableName(t).includes(searchInput)
    );

    const listedFunctions = functionsList.filter(f =>
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
      let tableLinks = [
        <li className={styles.noChildren} key="no-tables-1">
          <i>No tables/views available</i>
        </li>,
      ];

      const tables = {};
      tableList.map(t => {
        if (t.is_table_tracked) {
          tables[getTableName(t)] = t;
        }
      });

      const currentLocation = location.pathname;

      if (tableList && tableList.length) {
        tableLinks = Object.keys(tables)
          .sort()
          .map((tableName, i) => {
            const table = tables[tableName];

            let activeTableClass = '';
            if (
              tableName === currentTable &&
              currentLocation.indexOf(currentTable) !== -1
            ) {
              activeTableClass = styles.activeTable;
            }

            let gqlCompatibilityWarning = null;
            if (!gqlPattern.test(tableName)) {
              gqlCompatibilityWarning = (
                <span className={styles.add_mar_left_mid}>
                  <GqlCompatibilityWarning />
                </span>
              );
            }

            return (
              <li className={activeTableClass} key={i}>
                <Link to={getTableBrowseRoute(table)} data-test={tableName}>
                  <i
                    className={styles.tableIcon + ' fa fa-table'}
                    aria-hidden="true"
                  />
                  {displayTableName(table)}
                </Link>
                {gqlCompatibilityWarning}
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
        const functionHtml = listedFunctions.map((func, i) => {
          const funcName = getFunctionName(func);
          const isActive = funcName === currentFunction;

          return (
            <li className={isActive ? styles.activeTable : ''} key={'fn ' + i}>
              <Link to={getFunctionModifyRoute(func)} data-test={funcName}>
                <div
                  className={styles.display_inline + ' ' + styles.functionIcon}
                >
                  <img src={isActive ? functionSymbolActive : functionSymbol} />
                </div>
                {getFunctionName(func)}
              </Link>
            </li>
          );
        });

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
        showAddBtn={migrationMode}
        searchInput={getSearchInput()}
        heading={`Tables (${trackedTablesLength})`}
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
    currentTable: state.tables.currentTable,
    migrationMode: state.main.migrationMode,
    functionsList: state.tables.trackedFunctions,
    currentFunction: state.functions.functionName,
    serverVersion: state.main.serverVersion ? state.main.serverVersion : '',
    metadata: state.metadata,
  };
};

export default connect(mapStateToProps)(DataSubSidebar);
