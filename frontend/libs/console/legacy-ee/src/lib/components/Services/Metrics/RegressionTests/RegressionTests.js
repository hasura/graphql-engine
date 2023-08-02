import React, { useState } from 'react';
import { Tab, Tabs, TabList, TabPanel } from 'react-tabs';
import { Analytics, REDACT_EVERYTHING } from '@hasura/console-legacy-ce';

import { NewOperations } from './NewOperations';
import { ExistingTestsOperations } from './ExistingTestsOperations';
import { RunTestsWrapper } from './RunTestsWrapper';
import { PastRuns } from './PastRuns';
import { loadPATState } from '../../../AppState';
import { getMetricsUrl } from '../utils';

import styles from '../Metrics.module.scss';

export const RegressionTests = props => {
  const { metricsFQDN } = props;
  const [tabIndex, setTabIndex] = useState(0);
  const personalAccessToken = loadPATState();

  const metricsUrl = getMetricsUrl(metricsFQDN);

  const changeToNewTab = () => setTabIndex(1);

  const gotoTestSuite = () => setTabIndex(0);

  return (
    <Analytics name="MonitoringRegressionTests" {...REDACT_EVERYTHING}>
      <div className="infoWrapper">
        <Tabs
          selectedIndex={tabIndex}
          onSelect={i => setTabIndex(i)}
          className={styles.tabWrapper}
        >
          <TabList className={styles.tabListWrapper}>
            <Tab
              className={`${styles.tabList} ${
                tabIndex === 0 && styles.tabListActive
              }`}
            >
              Test suite
            </Tab>
            <Tab
              className={`${styles.tabList} ${
                tabIndex === 1 && styles.tabListActive
              }`}
            >
              New operations
            </Tab>
            <Tab
              className={`${styles.tabList} ${
                tabIndex === 2 && styles.tabListActive
              }`}
            >
              Run tests
            </Tab>
            <Tab
              className={`${styles.tabList} ${
                tabIndex === 3 && styles.tabListActive
              }`}
            >
              Past runs
            </Tab>
          </TabList>
          <TabPanel>
            <ExistingTestsOperations
              projectId={props.projectInfo.id}
              projectName={props.projectInfo.name}
              changeToNewTab={changeToNewTab}
            />
          </TabPanel>
          <TabPanel>
            <NewOperations
              projectId={props.projectInfo.id}
              projectName={props.projectInfo.name}
              gotoTestSuite={gotoTestSuite}
            />
          </TabPanel>
          <TabPanel>
            <RunTestsWrapper
              projectId={props.projectInfo.id}
              projectName={props.projectInfo.name}
              idToken={props.idToken}
              accessToken={props.accessToken}
              personalAccessToken={personalAccessToken}
              metricsUrl={metricsUrl}
            />
          </TabPanel>
          <TabPanel>
            <PastRuns
              projectId={props.projectInfo.id}
              projectName={props.projectInfo.name}
            />
          </TabPanel>
        </Tabs>
      </div>
    </Analytics>
  );
};

const mapStateToProps = (state, ownProps) => {
  const project = state.main.project;
  const idToken = state.main.oAuthResponse.id_token;
  const accessToken = state.main.oAuthResponse.access_token;
  return {
    ...ownProps,
    projectInfo: project,
    idToken,
    accessToken,
    metricsFQDN: project.metricsFQDN,
  };
};

export const regressionTestsConnector = connect =>
  connect(mapStateToProps)(RegressionTests);
