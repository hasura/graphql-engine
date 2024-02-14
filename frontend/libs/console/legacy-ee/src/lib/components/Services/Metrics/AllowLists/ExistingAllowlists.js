import React from 'react';
import BrowseAllowLists from './BrowseAllowLists';

import AllowListDataWrapper from './AllowListDataWrapper';

import ManageAllowList from './ManageAllowList';

const ExistingAllowlists = props => {
  const { collectionName, projectName, projectId } = props;
  return (
    <AllowListDataWrapper projectId={projectId} groupName={collectionName}>
      {({ data, refetch }) => {
        const { operation_groups: operationGroups } = data;
        const renderNext = () => {
          return (
            <ManageAllowList
              operationGroups={operationGroups}
              projectId={projectId}
              refetch={refetch}
              projectName={projectName}
              shouldCreateOperationGroup={operationGroups.length === 0}
            >
              {() => {
                return <BrowseAllowLists label={''} {...props} />;
              }}
            </ManageAllowList>
          );
        };
        return renderNext();
      }}
    </AllowListDataWrapper>
  );
};

export default ExistingAllowlists;
