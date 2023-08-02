/* eslint-disable no-underscore-dangle */
import React from 'react';
import { browserHistory } from 'react-router';
import { Analytics, REDACT_EVERYTHING } from '../../../features/Analytics';

import { Tabs } from '../../../new-components/Tabs';
import {
  useQueryCollections,
  QueryCollectionsOperations,
  QueryCollectionHeader,
} from '../../../features/QueryCollections';
import {
  AllowListSidebar,
  AllowListPermissions,
} from '../../../features/AllowLists';
import { EETrialCard, useEELiteAccess } from '../../../features/EETrial';

import PageContainer from '../../Common/Layout/PageContainer/PageContainer';
import { isProConsole } from '../../../utils/proConsole';
import globals from '../../../Globals';

interface AllowListDetailProps {
  params: {
    name: string;
    section: string;
  };
}

export const buildUrl = (name: string, section: string) =>
  `/api/allow-list/detail/${name}/${section}`;

export const pushUrl = (name: string, section: string) => {
  browserHistory.replace(buildUrl(name, section));
};

export const AllowListDetail: React.FC<AllowListDetailProps> = props => {
  const { name, section } = props.params;
  const { access: eeLiteAccess } = useEELiteAccess(globals);
  const {
    data: queryCollections,
    isLoading,
    isRefetching,
  } = useQueryCollections();

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

  const isFeatureActive = isProConsole(globals) || eeLiteAccess === 'active';
  const isFeatureSupported =
    isProConsole(globals) || eeLiteAccess !== 'forbidden';
  const isEELiteContext = eeLiteAccess !== 'forbidden';

  return (
    <Analytics name="AllowList" {...REDACT_EVERYTHING}>
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
            {isFeatureSupported ? (
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
                        {isFeatureActive ? (
                          <AllowListPermissions collectionName={name} />
                        ) : (
                          isEELiteContext && (
                            <div className="max-w-3xl">
                              <EETrialCard
                                id="allow-list-role-based-permission"
                                cardTitle="Looking to add role based permissions to your Allow List?"
                                cardText="Get production-ready today  with a 30-day free trial of Hasura EE, no credit card required."
                                buttonType="default"
                                eeAccess={eeLiteAccess}
                                horizontal
                              />
                            </div>
                          )
                        )}
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
    </Analytics>
  );
};
