import CommonTabLayout from '@/components/Common/Layout/CommonTabLayout/CommonTabLayout';
import React from 'react';
import {
  useIsFeatureFlagEnabled,
  availableFeatureFlagIds,
} from '@/features/FeatureFlags';
import { BreadCrumb } from '@/components/Common/Layout/BreadCrumb/BreadCrumb';

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

const useTabInfo = () => {
  const { enabled } = useIsFeatureFlagEnabled(
    availableFeatureFlagIds.remoteSchemaRelationshipsId
  );
  return enabled ? tabInfoWithRelationships : tabInfo;
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
  const tabsInfo = useTabInfo();
  return <CommonTabLayout tabsInfo={tabsInfo} {...props} />;
};
