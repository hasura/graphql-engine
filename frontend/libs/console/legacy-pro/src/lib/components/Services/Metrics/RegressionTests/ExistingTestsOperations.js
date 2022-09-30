import React from 'react';

import { BrowseRows } from './BrowseRows';
import { ContentWithTestSuite } from './ContentWithTestSuite';

/**
 * @typedef Props
 * @property {string} projectId
 * @property {string} projectName
 *
 * @param {Props} props
 */
export const ExistingTestsOperations = props => {
  return (
    <ContentWithTestSuite {...props}>
      {({ testSuiteId }) => <BrowseRows {...props} testSuiteId={testSuiteId} />}
    </ContentWithTestSuite>
  );
};
