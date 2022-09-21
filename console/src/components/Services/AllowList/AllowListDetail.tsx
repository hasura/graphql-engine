import React from 'react';
import { browserHistory } from 'react-router';

import { Tabs } from '@/new-components/Tabs';
import { useConsoleConfig } from '@/hooks/useEnvVars';
import {
  useQueryCollections,
  QueryCollectionsOperations,
  QueryCollectionHeader,
} from '@/features/QueryCollections';
import { AllowListSidebar, AllowListPermissions } from '@/features/AllowLists';

import PageContainer from '@/components/Common/Layout/PageContainer/PageContainer';

interface AllowListDetailProps {
  params: {
    name: string;
    section: string;
  };
}

export const buildUrl = (name: string, section: string) =>
  `/api/allow-list/detail/${name}/${section}`;

export const pushUrl = (name: string, section: string) => {
  browserHistory.push(buildUrl(name, section));
};

export const AllowListDetail: React.FC<AllowListDetailProps> = props => {
  const { name, section } = props.params;

  const {
    data: queryCollections,
    isLoading,
    isRefetching,
  } = useQueryCollections();

  const { type } = useConsoleConfig();

  const queryCollection = queryCollections?.find(
    ({ name: collectionName }) => collectionName === name
  );

  if (
    !isLoading &&
    !isRefetching &&
    queryCollections?.[0] &&
    (!name || !queryCollection)
  ) {
    // Redirect to first collection if no collection is selected or if the selected collection is not found
    pushUrl(queryCollections[0].name, section ?? 'operations');
  }

  return (
    <div className="flex flex-auto overflow-y-hidden h-[calc(100vh-35.49px-54px)]">
      <PageContainer
        helmet="Allow List Detail"
        leftContainer={
          <div className="bg-white border-r border-gray-300 h-full overflow-y-auto p-4">
            <AllowListSidebar
              onQueryCollectionCreate={newName => {
                pushUrl(newName, 'operations');
              }}
              buildQueryCollectionHref={(collectionName: string) =>
                buildUrl(collectionName, 'operations')
              }
              onQueryCollectionClick={url => browserHistory.push(url)}
              selectedCollectionQuery={name}
            />
          </div>
        }
      >
        <div className="h-full overflow-y-auto p-4">
          {queryCollection && (
            <div>
              <QueryCollectionHeader
                onRename={(_, newName) => {
                  pushUrl(newName, section);
                }}
                onDelete={() => {
                  if (queryCollections?.[0]?.name) {
                    pushUrl(queryCollections?.[0]?.name, 'operations');
                  }
                }}
                queryCollection={queryCollection}
              />
            </div>
          )}
          {type !== 'oss' ? (
            <Tabs
              value={section}
              onValueChange={value => {
                pushUrl(name, value);
              }}
              items={[
                {
                  value: 'operations',
                  label: 'Operations',
                  content: (
                    <div className="p-4">
                      <QueryCollectionsOperations collectionName={name} />
                    </div>
                  ),
                },
                {
                  value: 'permissions',
                  label: 'Permissions',
                  content: (
                    <div className="p-4">
                      <AllowListPermissions collectionName={name} />
                    </div>
                  ),
                },
              ]}
            />
          ) : (
            <QueryCollectionsOperations collectionName={name} />
          )}
        </div>
      </PageContainer>
    </div>
  );
};
