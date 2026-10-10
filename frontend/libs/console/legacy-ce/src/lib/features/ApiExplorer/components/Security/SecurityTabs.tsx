import React from 'react';
import { useNavigate } from 'react-router';
import { RightContainer } from '../../../../components/Common/Layout/RightContainer';
import { ApiSecurityTabEELiteWrapper } from '../../../EETrial';
import { useDocumentTitle } from '@hasura/shared/hooks';
import { Breadcrumbs, Tabs } from '@hasura/shared/ui';
import { Heading } from '@radix-ui/themes';

const appPrefix = `/api`;

const tabs = {
  api_limits: {
    display_text: 'API Limits',
  },
  introspection: {
    display_text: 'Schema Introspection',
  },
};

export const SecurityTabs: React.FC<{
  tabName: keyof typeof tabs;
  children?: React.ReactNode;
}> = ({ children, tabName }) => {
  const navigate = useNavigate();
  useDocumentTitle(`${tabs[tabName].display_text} - Hasura`);
  const breadCrumbs = [
    {
      title: 'Security Settings',
      url: `${appPrefix}/security/api_limits`,
    },
    {
      title: tabs[tabName].display_text,
    },
  ];
  return (
    <RightContainer>
      <ApiSecurityTabEELiteWrapper>
        <div className="mt-4">
          <Breadcrumbs items={breadCrumbs} />
          <div className="my-4">
            <Heading size="4">{tabs[tabName].display_text}</Heading>
          </div>
          <Tabs
            value={tabName}
            onValueChange={(newTab) =>
              navigate(`${appPrefix}/security/${newTab}`)
            }
            items={Object.entries(tabs).map(([value, { display_text }]) => ({
              value,
              label: display_text,
              content:
                value === tabName ? (
                  <div className="pt-4">{children}</div>
                ) : null,
            }))}
          />
        </div>
      </ApiSecurityTabEELiteWrapper>
    </RightContainer>
  );
};
