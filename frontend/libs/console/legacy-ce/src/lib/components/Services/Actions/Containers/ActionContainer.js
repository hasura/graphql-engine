import React from 'react';
import CommonTabLayout from '../../../Common/Layout/CommonTabLayout/CommonTabLayout';
import tabInfo from './actionTabs';
import { NotFoundError } from '../../../Error/PageNotFound';
import { appPrefix } from '../constants';
import { setCurrentAction } from '../reducer';
import { findAction } from '../utils';

const ActionContainer = ({
  params: { actionName },
  children,
  allActions,
  tabName,
  dispatch,
}) => {
  React.useEffect(() => {
    dispatch(setCurrentAction(actionName));
    return () => {
      dispatch(setCurrentAction(''));
    };
  }, [actionName]);

  const currentAction = findAction(allActions, actionName);

  if (!currentAction) {
    dispatch(setCurrentAction(''));
    throw new NotFoundError();
  }

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
      url: `${appPrefix}/manage/${actionName}/modify`,
    },
    {
      title: tabName,
      url: '',
    },
  ];

  const childrenWithProps = React.Children.map(children, child =>
    React.cloneElement(child, { currentAction })
  );

  return (
    <div>
      <CommonTabLayout
        appPrefix={appPrefix}
        currentTab={tabName}
        heading={actionName}
        tabsInfo={tabInfo}
        breadCrumbs={breadCrumbs}
        baseUrl={`${appPrefix}/manage/${actionName}`}
      />
      <div className="pt-xl bootstrap-jail">{childrenWithProps}</div>
    </div>
  );
};

export default ActionContainer;
