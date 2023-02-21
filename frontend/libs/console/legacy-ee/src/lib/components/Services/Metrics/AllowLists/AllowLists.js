import React, { useState, useEffect } from 'react';
import PropTypes from 'prop-types';
import { Analytics, REDACT_EVERYTHING } from '@hasura/console-legacy-ce';

import { allowListOperationGroupName } from './constants';

import ExistingAllowlists from './ExistingAllowlists';

import AddOperations from './AddOperations';

import useRefetch from '../Common/useRefetch';

import { Tab, Tabs, TabList, TabPanel } from 'react-tabs';

import 'react-tabs/style/react-tabs.css';
import { refetchMetadata as refetchMetadataAction } from '../../../Main/Actions';
import { isAdmin as _isAdmin } from '../utils';

import styles from '../Metrics.module.scss';
/*
 * Lists
 * */

/*
const styles = require('../Metrics.module.scss');
const AllowListsInstructions = () => {
  return (
    <div>
      <div className={styles.wd56}>
        If GraphQL Engine is started with the{' '}
        <code>HASURA_GRAPHQL_ENABLE_ALLOWLIST</code> env var or the{' '}
        <code>--enable-allowlist</code> flag set to <i>true</i>, only queries
        added to the allow-list will be allowed to be executed.&nbsp;
        <a
          href="https://docs.hasura.io/1.0/graphql/manual/deployment/allow-list.html"
          target="_blank"
          rel="noopener noreferrer"
        >
          <i>(Read more)</i>
        </a>
      </div>
      <div className={styles.add_mar_top}>
        <b>Notes</b>
        <div className={styles.subsection}>
          <ul className={styles.ul_left_small + ' ' + styles.add_mar_top_small}>
            <li>
              This section will let you combine operations that has ever
              happened with your GraphQL engine into a collection.
            </li>
            <li>
              GraphQL engine is not configured with a collection by default. You
              can create a collection and sync operations happened with your
              GraphQL engine by clicking on the <code>Generate allow list</code>{' '}
              button.
            </li>
            <li>
              You can use the{' '}
              <code>
                <img
                  src={syncIcon}
                  alt="Sync operations with your collection"
                />
              </code>{' '}
              to sync operations in your GraphQL engine with your collection.
              Its a convenient way of keeping the collection up to date with the
              operations happened with the server.
            </li>
            <li>
              Once the operations are added to a collection, you can apply the
              same to your GraphQL engine using the <code>Apply</code> button
              below.
            </li>
            <li>
              This list will show all the operations ever happened against your
              server. It will also indicate if the operation is added to the
              collection or not using{' '}
              <code>
                <img src={addIcon} alt="Add operation icon" />
              </code>{' '}
              and{' '}
              <code>
                <img src={removeIcon} alt="Remove operation icon" />
              </code>{' '}
              respectively. You can add/remove operations as per convenience.
            </li>
          </ul>
        </div>
      </div>
    </div>
  );
};
*/

const defaultMetadata = {
  allowedListCount: null,
  newListCount: null,
};

const AllowLists = props => {
  const [tabIndex, updateTabIndex] = useState(0);
  const [metadataState, updateMetadata] = useState(defaultMetadata);

  const { allowedListCount, newListCount } = metadataState;

  const {
    projectInfo,
    RenderLink,
    dispatch,
    refetchMetadata,
    metadata,
    privileges,
  } = props;

  const isAdmin = _isAdmin(privileges);

  useEffect(() => {
    if (
      metadata?.query_collections === null &&
      metadata?.loading !== true &&
      isAdmin
    ) {
      // if there is no query_collections, this will become undefined, null is from the default client state
      refetchMetadata();
    }
  }, [metadata]);
  const changeToAllowedOperations = () => updateTabIndex(0);
  const changeToNewTab = () => updateTabIndex(1);

  const changeTabs = {
    changeToAllowedOperations: changeToAllowedOperations,
    changeToNewTab: changeToNewTab,
  };

  const [refetchState, update] = useRefetch({});

  const refetchMeta = {
    refetchState,
    update,
  };

  const { id: projectId, name: projectName } = projectInfo;
  return (
    <Analytics name="MonitoringAllowLists" {...REDACT_EVERYTHING}>
      <div className="infoWrapper">
        <Tabs
          selectedIndex={tabIndex}
          onSelect={t => updateTabIndex(t)}
          className={styles.tabWrapper}
        >
          <TabList className={styles.tabListWrapper}>
            <Tab
              className={
                styles.tabList +
                ' ' +
                (tabIndex === 0 ? styles.tabListActive : '')
              }
            >
              Allowed Operations{' '}
              {allowedListCount !== null && `(${allowedListCount})`}
            </Tab>
            <Tab
              className={
                styles.tabList +
                ' ' +
                (tabIndex === 1 ? styles.tabListActive : '')
              }
            >
              New Operations {newListCount !== null && `(${newListCount})`}
            </Tab>
          </TabList>
          <TabPanel>
            <ExistingAllowlists
              {...props}
              dispatch={dispatch}
              collectionName={allowListOperationGroupName}
              projectId={projectId}
              projectName={projectName}
              RenderLink={RenderLink}
              {...refetchMeta}
              {...changeTabs}
              updateMetadata={d =>
                updateMetadata(s => {
                  return { ...s, allowedListCount: d };
                })
              }
            />
          </TabPanel>
          <TabPanel>
            <AddOperations
              collectionName={allowListOperationGroupName}
              projectId={projectId}
              projectName={projectName}
              RenderLink={RenderLink}
              {...props}
              {...refetchMeta}
              {...changeTabs}
              updateMetadata={d =>
                updateMetadata(s => {
                  return { ...s, newListCount: d };
                })
              }
            />
          </TabPanel>
        </Tabs>
      </div>
    </Analytics>
  );
};

AllowLists.propTypes = {
  projectInfo: PropTypes.object.isRequired,
  privileges: PropTypes.array.isRequired,
};

export { AllowLists };

const mapStateToProps = (state, ownProps) => {
  const { project, metadata } = state.main;
  return {
    ...ownProps,
    projectInfo: project || {},
    privileges: project.privileges || [],
    metadata,
  };
};
const mapDispatchToProps = dispatch => ({
  refetchMetadata: () => dispatch(refetchMetadataAction()),
  dispatch,
});

const allowListsConnector = connect =>
  connect(mapStateToProps, mapDispatchToProps)(AllowLists);

export default allowListsConnector;
