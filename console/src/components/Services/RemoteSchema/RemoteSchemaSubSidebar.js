import React from 'react';
import { Link } from 'react-router';

import LeftSubSidebar from '../../Common/Layout/LeftSubSidebar/LeftSubSidebar';
import styles from '../../Common/Layout/LeftSubSidebar/LeftSubSidebar.scss';

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
  const unifiedList = [].concat(inconsistentObjects, dataList);
  function tableSearch(e) {
    const searchTerm = e.target.value;
    filterItem(unifiedList, searchTerm);
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
    if (_dataList.length === 0 && inconsistentObjects.length === 0) {
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

          return (
            <li
              className={activeTableClass}
              key={i}
              data-test={`remote-schema-sidebar-links-${i + 1}`}
            >
              <Link
                to={appPrefix + '/manage/' + d.name + '/details'}
                data-test={d.name}
              >
                <i
                  className={styles.tableIcon + ' fa fa-code-fork'}
                  aria-hidden="true"
                />
                {d.name}
              </Link>
            </li>
          );
        });
      }

      if (inconsistentObjects.length > 0) {
        const inconChildren = inconsistentObjects.map((d, i) => {
          let activeTableClass = '';
          if (
            d.name === viewRemoteSchema &&
            location.pathname.includes(viewRemoteSchema)
          ) {
            activeTableClass = styles.activeLink;
          }

          return (
            <li
              className={activeTableClass}
              key={i}
              data-test={`remote-schema-sidebar-links-${i + 1}`}
            >
              <Link
                to={appPrefix + '/manage/' + d.name + '/details'}
                data-test={d.name}
              >
                <i
                  className={styles.tableIcon + ' fa fa-exclamation-triangle'}
                  aria-hidden="true"
                  title={d.reason}
                />
                {d.name}
              </Link>
            </li>
          );
        });

        if (childList.length) {
          childList.concat(inconChildren);
        } else {
          childList = inconChildren;
        }
      }
    }

    return childList;
  };

  return (
    <LeftSubSidebar
      showAddBtn={!main.readOnlyMode}
      searchInput={getSearchInput()}
      heading={`Remote Schemas (${dataList.length})`}
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
