import React from 'react';
import styles from '../Metrics.module.scss';
import { BrowseAllOperations } from './BrowseAllOperations';
import { ContentWithTestSuite } from './ContentWithTestSuite';

export const NewOperations = props => {
  return (
    <div className={styles.padding_top_20}>
      <ContentWithTestSuite {...props}>
        {({ testSuiteId }) => (
          <BrowseAllOperations {...props} testSuiteId={testSuiteId} />
        )}
      </ContentWithTestSuite>
    </div>
  );
};
