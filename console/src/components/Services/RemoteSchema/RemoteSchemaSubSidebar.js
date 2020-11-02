import React from 'react';
import { Link } from 'react-router';

import LeftSubSidebar from '../../Common/Layout/LeftSubSidebar/LeftSubSidebar';
import styles from '../../Common/Layout/LeftSubSidebar/LeftSubSidebar.scss';
import WarningSymbol from '../../Common/WarningSymbol/WarningSymbol';

const RemoteSchemaSubSidebar = ({
  appPrefix,
  dataList,
  filtered,
  searchQuery,
  location,
  filterItem,
  viewRemoteSchema,
  main,
  ...props
}) => {
  const { inconsistentObjects } = props.metadata;
  const inconsistentRemoteSchemas = inconsistentObjects.filter(
    inconObject => inconObject.type === 'remote_schema'
  );

  function tableSearch(e) {
    const searchTerm = e.target.value;
    filterItem(dataList, searchTerm);
  }

  const getSearchInput = () => {
    return (
      <input
        type="text"
        onChange={tableSearch.bind(this)}
        className="form-control"
        placeholder="search remote schemas"
        data-test="search-remote-schemas"
      />
    );
  };

  const getChildList = () => {
    const _dataList = searchQuery ? filtered : dataList;

    let childList = [];
    if (_dataList.length === 0) {
      childList = (
        <li
          className={styles.noChildren}
          data-test="remote-schema-sidebar-no-schemas"
        >
          <i>No remote schemas available</i>
        </li>
      );
    } else {
      if (_dataList.length > 0) {
        childList = _dataList.map((d, i) => {
          let activeTableClass = '';

          if (
            d.name === viewRemoteSchema &&
            location.pathname.includes(viewRemoteSchema)
          ) {
            activeTableClass = styles.activeLink;
          }

          const inconsistentCurrentSchema = inconsistentRemoteSchemas.find(
            elem => elem.definition.name === d.name
          );

          return (
            <li
              className={activeTableClass}
              key={i}
              data-test={`remote-schema-sidebar-links-${i + 1}`}
            >
              <Link
                to={`${appPrefix}/manage/${d.name}/details`}
                data-test={d.name}
              >
                <i
                  className={`${styles.tableIcon} fa fa-code-fork`}
                  aria-hidden="true"
                />
                {d.name}
                {inconsistentCurrentSchema ? (
                  <WarningSymbol
                    customStyle={styles.padLeft4}
                    tooltipText={
                      'This remote schema is in an inconsistent state. ' +
                      'Fields from this remote schema are currently not exposed over the GraphQL API'
                    }
                    tooltipPlacement="right"
                  />
                ) : null}
              </Link>
            </li>
          );
        });
      }
    }

    return childList;
  };

  return (
    <LeftSubSidebar
      showAddBtn={!main.readOnlyMode}
      searchInput={getSearchInput()}
      heading={`Remote Schemas (${dataList.length ?? 0})`}
      addLink={`${appPrefix}/manage/add`}
      addLabel={'Add'}
      addTestString={'remote-schema-sidebar-add-table'}
      childListTestString={'remote-schema-table-links'}
    >
      {getChildList()}
    </LeftSubSidebar>
  );
};

export default RemoteSchemaSubSidebar;
