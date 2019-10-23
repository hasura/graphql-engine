import React from 'react';
import { Link } from 'react-router';

import LeftSubSidebar from '../../../Common/Layout/LeftSubSidebar/LeftSubSidebar';
import styles from '../../../Common/Layout/LeftSubSidebar/LeftSubSidebar.scss';

const dataList = [{ name: 'action1' }, { name: 'action2' }];

const LeftSidebar = ({ appPrefix, location }) => {
  function actionsSearch(e) {
    const searchTerm = e.target.value;
    console.log('searching', searchTerm);
  }

  const getSearchInput = () => {
    return (
      <input
        type="text"
        onChange={actionsSearch}
        className="form-control"
        placeholder="search actions"
        data-test="search-actions"
      />
    );
  };

  const getChildList = () => {
    const _dataList = [{ name: 'action1' }, { name: 'action2' }];

    let childList;
    if (_dataList.length === 0) {
      childList = (
        <li
          className={styles.noChildren}
          data-test="actions-sidebar-no-actions"
        >
          <i>No actions available</i>
        </li>
      );
    } else {
      childList = _dataList.map((d, i) => {
        let activeTableClass = '';
        if (d.name === 'action1' && location.pathname.includes('action1')) {
          activeTableClass = styles.activeLink;
        }

        return (
          <li
            className={activeTableClass}
            key={i}
            data-test={`action-sidebar-links-${i + 1}`}
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

    return childList;
  };

  return (
    <LeftSubSidebar
      showAddBtn
      searchInput={getSearchInput()}
      heading={`Actions (${dataList.length})`}
      addLink={`${appPrefix}/manage/add`}
      addLabel={'Create'}
      addTestString={'actions-sidebar-add-table'}
      childListTestString={'actions-table-links'}
    >
      {getChildList()}
    </LeftSubSidebar>
  );
};

export default LeftSidebar;
