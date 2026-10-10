import React from 'react';
import { useNavigate, useParams } from 'react-router';
import { Flex } from '@radix-ui/themes';
import { Analytics, REDACT_EVERYTHING } from '@hasura/shared/analytics';
import { Tabs, PageContainer } from '@hasura/shared/ui';
import {
  useQueryCollections,
  QueryCollectionsOperations,
  QueryCollectionHeader,
} from '../../../QueryCollections';
import { AllowListSidebar, AllowListPermissions } from '../..';
import { EETrialCard, useEELiteAccess } from '../../../EETrial';
import { isProConsole } from '@hasura/shared/utils';
import { useAppContext } from '@hasura/shared/context';

export const buildUrl = (name: string, section: string) =>
  `/api/allow-list/detail/${name}/${section}`;

export const AllowListDetail: React.FC = () => {
  const params = useParams<{ name: string; section: string }>();
  const navigate = useNavigate();
  const name = params.name ?? '';
  const section = params.section ?? '';
  const { envVars } = useAppContext();
  const { access: eeLiteAccess } = useEELiteAccess();
  const {
    data: queryCollections,
    isLoading,
    isRefetching,
  } = useQueryCollections();

  const queryCollection = queryCollections?.find(
    ({ name: collectionName }) => collectionName === name,
  );

  if (
    !isLoading &&
    !isRefetching &&
    queryCollections?.[0] &&
    (!name || !queryCollection)
  ) {
    // Redirect to first collection if no collection is selected or if the selected collection is not found
    navigate(buildUrl(queryCollections[0].name, section ?? 'operations'));
  }

  const isFeatureActive = isProConsole(envVars) || eeLiteAccess === 'active';
  const isFeatureSupported =
    isProConsole(envVars) || eeLiteAccess !== 'forbidden';
  const isEELiteContext = eeLiteAccess !== 'forbidden';

  return (
    <Analytics name="AllowList" {...REDACT_EVERYTHING}>
      <Flex className="flex-auto overflow-y-hidden h-[calc(100vh-35.49px-54px)]">
        <PageContainer
          helmet="Allow List Detail"
          leftContainer={
            <div className="border-r border-gray-300 h-full overflow-y-auto p-4">
              <AllowListSidebar
                onQueryCollectionCreate={(newName) => {
                  navigate(buildUrl(newName, 'operations'));
                }}
                buildQueryCollectionHref={(collectionName: string) =>
                  buildUrl(collectionName, 'operations')
                }
                onQueryCollectionClick={(url) => navigate(url)}
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
                    navigate(buildUrl(newName, section));
                  }}
                  onDelete={() => {
                    if (queryCollections?.[0]?.name) {
                      navigate(
                        buildUrl(queryCollections?.[0]?.name, 'operations'),
                      );
                    }
                  }}
                  queryCollection={queryCollection}
                />
              </div>
            )}
            {isFeatureSupported ? (
              <Tabs
                value={section}
                onValueChange={(value) => {
                  navigate(buildUrl(name, value));
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
      </Flex>
    </Analytics>
  );
};
