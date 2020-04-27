import React from 'react';
import CommonTabLayout from '../../../Common/Layout/CommonTabLayout/CommonTabLayout';
import tabInfo from './scheduledTriggerTabs';
import { findScheduledTrigger } from '../ScheduledTriggers/utils';
import { NotFoundError } from '../../../Error/PageNotFound';
import { appPrefix } from '../constants';
import { setCurrentTrigger } from '../reducer';
import { Triggers } from '../Types';
import styles from '../Triggers.scss';

interface ScheduledTriggerProps extends React.ComponentProps<'div'> {
  triggerName: string;
  allTriggers: Triggers;
  tabName: string;
  dispatch: any;
}

const ActionContainer = ({
  triggerName,
  children,
  allTriggers,
  tabName,
  dispatch,
}: ScheduledTriggerProps) => {
  React.useEffect(() => {
    dispatch(setCurrentTrigger(triggerName));
    return () => {
      dispatch(setCurrentTrigger(''));
    };
  }, [triggerName]);

  const currentTrigger = findScheduledTrigger(allTriggers, triggerName);

  if (!currentTrigger) {
    dispatch(setCurrentTrigger(''));
    throw new NotFoundError();
  }

  const breadCrumbs = [
    {
      title: 'Triggers',
      url: appPrefix,
    },
    {
      title: 'Manage',
      url: `${appPrefix}/scheduled/manage`,
    },
    {
      title: triggerName,
      url: `${appPrefix}/scheduled/${triggerName}/${tabName}`,
    },
    {
      title: tabName,
      url: '',
    },
  ];

  const childrenWithProps = React.Children.map(children, child =>
    React.cloneElement(child as React.ReactElement<any>, {
      currentTrigger,
    })
  );

  return (
    <div
      className={styles.view_stitch_schema_wrapper + ' ' + styles.addWrapper}
    >
      <CommonTabLayout
        appPrefix={appPrefix}
        currentTab={tabName}
        heading={triggerName}
        tabsInfo={tabInfo}
        breadCrumbs={breadCrumbs}
        baseUrl={`${appPrefix}/scheduled/${triggerName}`}
        showLoader={false}
        testPrefix={`${triggerName}-container-tabs`}
      />
      <div className={styles.add_pad_top}>{childrenWithProps}</div>
    </div>
  );
};

export default ActionContainer;
