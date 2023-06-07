import debounce from 'lodash/debounce';
import React from 'react';
import { IndicatorCard } from '../../../../new-components/IndicatorCard';
import { useServerConfig } from '../../../../hooks';
import globals from '../../../../Globals';
import { isProConsole } from '../../../../utils/proConsole';
import { useEELiteAccess } from '../../../../features/EETrial';
import { LearnMoreLink } from '../../../../new-components/LearnMoreLink';
import { AllowListSidebarHeader } from './AllowListSidebarHeader';
import { QueryCollectionList } from './QueryCollectionList';
import { AllowListSidebarSearchForm } from './AllowListSidebarSearchForm';

interface AllowListSidebarProps {
  selectedCollectionQuery: string;
  buildQueryCollectionHref: (name: string) => string;
  onQueryCollectionClick: (url: string) => void;
  onQueryCollectionCreate: (name: string) => void;
}

export const AllowListSidebar: React.FC<AllowListSidebarProps> = props => {
  const {
    selectedCollectionQuery,
    buildQueryCollectionHref,
    onQueryCollectionClick,
    onQueryCollectionCreate,
  } = props;
  const [search, setSearch] = React.useState('');
  const debouncedSearch = React.useMemo(() => debounce(setSearch, 300), []);

  const { access: eeLiteAccess } = useEELiteAccess(globals);
  const allowQueryCollectionsCreation =
    isProConsole(globals) || eeLiteAccess === 'active';

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
      <AllowListSidebarSearchForm
        setSearch={(searchString: string) => debouncedSearch(searchString)}
      />
      <QueryCollectionList
        buildHref={buildQueryCollectionHref}
        onClick={onQueryCollectionClick}
        selectedCollectionQuery={selectedCollectionQuery}
        search={search}
      />
      {renderInstructions && (
        <IndicatorCard status="info">
          <p>
            Want to enable your allow list? You can set{' '}
            <span className="text-red-600 font-mono bg-red-50 rounded px-1.5 py-0.5 break-all">
              HASURA_GRAPHQL_ENABLE_ALLOWLIST
            </span>{' '}
            to{' '}
            <span className="text-red-600 font-mono bg-red-50 rounded px-1.5 py-0.5">
              true
            </span>{' '}
            so that your API will only allow accepted pre-selected operations.
            <LearnMoreLink href="https://hasura.io/docs/latest/security/allow-list/#enable-allow-list" />
          </p>
        </IndicatorCard>
      )}
    </div>
  );
};
