import React from 'react';
import { IndicatorCard, Input, LearnMoreLink, Text } from '@hasura/shared/ui';
import { isProConsole } from '@hasura/shared/utils';
import { useEELiteAccess } from '../../../../features/EETrial';
import { AllowListSidebarHeader } from './AllowListSidebarHeader';
import { QueryCollectionList } from './QueryCollectionList';
import { useServerConfig } from '@hasura/metadata/api';
import { useAppContext } from '@hasura/shared/context';
import { Code } from '@radix-ui/themes';
import { FaSearch } from 'react-icons/fa';

interface AllowListSidebarProps {
  selectedCollectionQuery: string;
  buildQueryCollectionHref: (name: string) => string;
  onQueryCollectionClick: (url: string) => void;
  onQueryCollectionCreate: (name: string) => void;
}

export const AllowListSidebar: React.FC<AllowListSidebarProps> = (props) => {
  const {
    selectedCollectionQuery,
    buildQueryCollectionHref,
    onQueryCollectionClick,
    onQueryCollectionCreate,
  } = props;
  const { envVars } = useAppContext();
  const [search, setSearch] = React.useState('');

  const { access: eeLiteAccess } = useEELiteAccess();
  const allowQueryCollectionsCreation =
    isProConsole(envVars) || eeLiteAccess === 'active';

  const { data: configData, isLoading: isConfigLoading } = useServerConfig();

  const renderInstructions =
    !isConfigLoading && !configData?.is_allow_list_enabled;

  return (
    <div>
      <AllowListSidebarHeader
        onQueryCollectionCreate={
          allowQueryCollectionsCreation ? onQueryCollectionCreate : undefined
        }
      />
      <div className="mb-4">
        <Input
          placeholder="Search Collections..."
          icon={FaSearch}
          value={search}
          onChange={(ev) => {
            setSearch(ev.target.value);
          }}
        />
      </div>
      <QueryCollectionList
        buildHref={buildQueryCollectionHref}
        onClick={onQueryCollectionClick}
        selectedCollectionQuery={selectedCollectionQuery}
        search={search}
      />
      {renderInstructions && (
        <IndicatorCard status="info" size="1">
          <Text>
            Want to enable your allow list? You can set{' '}
            <Code color="red" size="1">
              HASURA_GRAPHQL_ENABLE_ALLOWLIST
            </Code>{' '}
            to <Code color="red">true</Code> so that your API will only allow
            accepted pre-selected operations.{' '}
            <LearnMoreLink href="https://hasura.io/docs/latest/security/allow-list/#enable-allow-list" />
          </Text>
        </IndicatorCard>
      )}
    </div>
  );
};
