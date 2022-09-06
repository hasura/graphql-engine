import debounce from 'lodash.debounce';
import React from 'react';
import { IndicatorCard } from '@/new-components/IndicatorCard';
import { useServerConfig } from '@/hooks';
import { AllowListSidebarHeader } from './AllowListSidebarHeader';
import { QueryCollectionList } from './QueryCollectionList';
import { AllowListSidebarSearchForm } from './AllowListSidebarSearchForm';

interface AllowListSidebarProps {
  selectedCollectionQuery: string;
}

export const AllowListSidebar: React.FC<AllowListSidebarProps> = props => {
  const { selectedCollectionQuery } = props;
  const [search, setSearch] = React.useState('');
  const debouncedSearch = React.useMemo(() => debounce(setSearch, 300), []);

  const { data: configData, isLoading: isConfigLoading } = useServerConfig();

  const renderInstructions =
    !isConfigLoading && !configData?.is_allow_list_enabled;

  return (
    <div>
      <AllowListSidebarHeader />
      <AllowListSidebarSearchForm
        setSearch={(searchString: string) => debouncedSearch(searchString)}
      />
      <QueryCollectionList
        selectedCollectionQuery={selectedCollectionQuery}
        search={search}
      />
      {renderInstructions && (
        <IndicatorCard status="info">
          <p>
            Want to enable your allow list? You can set{' '}
            <span className="text-red-600 font-mono bg-red-50 rounded px-1.5 py-0.5">
              HASURA_GRAPHQL_ENABLE_ALLOWLIST
            </span>{' '}
            to{' '}
            <span className="text-red-600 font-mono bg-red-50 rounded px-1.5 py-0.5">
              true
            </span>{' '}
            so that your API will only allow accepted pre-selected operations.
          </p>
        </IndicatorCard>
      )}
    </div>
  );
};
