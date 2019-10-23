import React from 'react';
import CommonTabLayout from '../../../Common/Layout/CommonTabLayout/CommonTabLayout';
import tabInfo from './tabs';
import { NotFoundError } from '../../../Error/PageNotFound';
import { appPrefix } from '../constants';

const TabContainer = ({ params, children, allActions, tabName }) => {
  const { actionName } = params;
  // if action name absent, push to prefixUrl else set action
  // if changes, change

  const currentAction = allActions.find(a => a.name === actionName);
  if (!currentAction) {
    throw new NotFoundError();
  }

  const styles = require('../Actions.scss');

  const breadCrumbs = [
    {
      title: 'Actions',
      url: appPrefix,
    },
    {
      title: 'Manage',
      url: `${appPrefix}/manage`,
    },
    {
      title: actionName,
      url: `${appPrefix}/manage/${actionName}/details`,
    },
    {
      title: tabName,
      url: '',
    },
  ];
  return (
    <div
      className={styles.view_stitch_schema_wrapper + ' ' + styles.addWrapper}
    >
      <CommonTabLayout
        appPrefix={appPrefix}
        currentTab="details"
        heading={actionName}
        tabsInfo={tabInfo}
        breadCrumbs={breadCrumbs}
        baseUrl={`${appPrefix}/manage/${actionName}`}
      />
      {children}
    </div>
  );
};

export default TabContainer;
