import React from 'react';
import { fetchOperationGroup } from './graphql.queries';

import { useQuery } from '@apollo/react-hooks';

/*
 * User lands on this page:
 *  1) Operation group doesn't exist in the first place. Can the user create it and do something with it somehow?
 *     - User should be provided with the details of what this feature is all about and what will happen if the user clicks on the button.
 *     - Once user clicks the button he should be shown a loading icon indicating process has been started and should be notified as and when required
 *     - Once an operation group is created, user should see a message saying `Syncing operations` post that it should refetch operation group and operations belonging to a particular group. Once that is completed something should happen which will show the list of operations and actions on them.
 *  2) Operation group for the allow list exists but it doesn't have any operations synced with it yet or has some operations.
 * */

const AllowListDataWrapper = ({ projectId, groupName, children }) => {
  const { loading, error, data, refetch } = useQuery(fetchOperationGroup, {
    variables: {
      groupName: groupName,
      projectId,
    },
    fetchPolicy: 'network-only',
  });
  const renderBody = () => {
    if (loading) {
      return <div>Getting operations...</div>;
    }
    if (error) {
      return <div>Error loading operations: ${error.toString()}</div>;
    }
    return children({ data, refetch });
  };
  return renderBody();
};

export default AllowListDataWrapper;
