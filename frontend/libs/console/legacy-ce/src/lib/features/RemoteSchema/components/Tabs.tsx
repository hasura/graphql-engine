import React from 'react';
import { useNavigate } from 'react-router';
import { BreadcrumbItem, Breadcrumbs, Tabs as UiTabs } from '@hasura/shared/ui';
import { Heading } from '@radix-ui/themes';

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
  relationships: {
    display_text: 'Relationships',
  },
};

interface TabsProps {
  breadCrumbs: BreadcrumbItem[];
  heading: React.ReactNode;
  currentTab: string;
  baseUrl: string;
}

export const Tabs = ({
  breadCrumbs,
  heading,
  currentTab,
  baseUrl,
}: TabsProps) => {
  const navigate = useNavigate();

  return (
    <div className="py-6">
      <Breadcrumbs items={breadCrumbs} />
      <div className="mt-4">
        <Heading size="5">{heading}</Heading>
      </div>
      <UiTabs
        value={currentTab}
        onValueChange={(newTab) => navigate(`${baseUrl}/${newTab}`)}
        items={Object.entries(tabInfo).map(([value, info]) => ({
          value,
          label: info.display_text,
          content: null,
        }))}
      />
    </div>
  );
};
