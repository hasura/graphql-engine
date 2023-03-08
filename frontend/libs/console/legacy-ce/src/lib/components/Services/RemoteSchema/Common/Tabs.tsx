import CommonTabLayout from '../../../Common/Layout/CommonTabLayout/CommonTabLayout';
import React from 'react';
import { BreadCrumb } from '../../../Common/Layout/BreadCrumb/BreadCrumb';

const tabInfo = {
  details: {
    display_text: 'Details',
  },
  modify: {
    display_text: 'Modify',
  },
  permissions: {
    display_text: 'Permissions',
  },
};

const tabInfoWithRelationships = {
  ...tabInfo,
  relationships: {
    display_text: 'Relationships',
  },
};
interface TabsProps {
  breadCrumbs: BreadCrumb[];
  heading: React.ReactNode;
  appPrefix: string;
  currentTab: string;
  baseUrl: string;
  showLoader: boolean;
  testPrefix: string;
  subHeading?: React.ReactNode;
}

export const Tabs = (props: TabsProps) => {
  return <CommonTabLayout tabsInfo={tabInfoWithRelationships} {...props} />;
};
