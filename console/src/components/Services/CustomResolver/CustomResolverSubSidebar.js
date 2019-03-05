import React from 'react';

import LeftSubSidebar from '../../Common/Layout/LeftSubSidebar/LeftSubSidebar';

const CustomResolverSubSidebar = ({
  appPrefix,
  listItemTemplate,
  dataList,
  filtered,
  searchQuery,
  location,
  filterItem,
  viewResolver,
  migrationMode,
}) => {
  const styles = require('../../Common/Layout/LeftSubSidebar/LeftSubSidebar.scss');

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

    return listItemTemplate(_dataList, styles, location, viewResolver);
  };

  return (
    <LeftSubSidebar
      migrationMode={migrationMode}
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

export default CustomResolverSubSidebar;
