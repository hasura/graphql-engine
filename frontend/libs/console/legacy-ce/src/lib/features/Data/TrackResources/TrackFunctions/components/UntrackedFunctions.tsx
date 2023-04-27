import { SlOptionsVertical } from 'react-icons/sl';
import Skeleton from 'react-loading-skeleton';
import { Button } from '../../../../../new-components/Button';
import { CardedTable } from '../../../../../new-components/CardedTable';
import { DropdownMenu } from '../../../../../new-components/DropdownMenu';
import { IndicatorCard } from '../../../../../new-components/IndicatorCard';
import { LearnMoreLink } from '../../../../../new-components/LearnMoreLink';
import { Feature } from '../../../../DataSource';
import { useInvalidateMetadata } from '../../../../hasura-metadata-api';
import { FunctionDisplayName } from './FunctionDisplayName';

import React, { useState } from 'react';
import { TrackableListMenu } from '../../components/TrackableListMenu';
import { usePaginatedSearchableList } from '../../hooks';
import { useUntrackedFunctions } from '../hooks/useUntrackedFunctions';
import { hasuraToast } from '../../../../../new-components/Toasts';
import { useTrackFunction } from '../../../hooks/useTrackFunction';
import { QualifiedFunction } from '../../../../hasura-metadata-types';
import { DisplayToastErrorMessage } from '../../../components/DisplayErrorMessage';
import { useHasuraAlert } from '../../../../../new-components/Alert';

export type UntrackedFunctionsProps = {
  dataSourceName: string;
};

type AllowedFunctionTypes = 'mutation' | 'query' | 'root_field';

export const UntrackedFunctions = (props: UntrackedFunctionsProps) => {
  const { dataSourceName } = props;

  const { data: untrackedFunctions = [], isLoading } =
    useUntrackedFunctions(dataSourceName);

  const functionsWithId = React.useMemo(() => {
    if (Array.isArray(untrackedFunctions)) {
      return untrackedFunctions.map(f => ({ ...f, id: f.name }));
    } else {
      return [];
    }
  }, [untrackedFunctions]);

  const invalidateMetadata = useInvalidateMetadata();

  const [activeRow, setActiveRow] = useState<number | undefined>();
  const [activeOperation, setActiveOperation] =
    useState<AllowedFunctionTypes>();

  const { hasuraConfirm } = useHasuraAlert();

  const { trackFunction, isLoading: isTrackingInProgress } = useTrackFunction({
    dataSourceName,
    onSuccess: () => {
      hasuraToast({
        type: 'success',
        title: 'Success',
        message: `Tracked object successfully`,
      });
    },
    onError: err => {
      hasuraToast({
        type: 'error',
        title: err.name,
        children: <DisplayToastErrorMessage message={err.message} />,
      });
    },
  });

  const listProps = usePaginatedSearchableList({
    data: functionsWithId,
    searchFn: (query, item) => {
      return item.name.toLowerCase().includes(query.toLowerCase());
    },
  });

  if (isLoading) return <Skeleton count={5} height={20} className="mb-1" />;

  if (untrackedFunctions === Feature.NotImplemented) return null;

  if (!untrackedFunctions.length)
    return (
      <IndicatorCard status="info" headline="No untracked functions found">
        We couldn't find any compatible functions in your database that can be
        tracked in Hasura.{' '}
        <LearnMoreLink href="https://hasura.io/docs/latest/schema/postgres/postgres-guides/functions/" />
      </IndicatorCard>
    );

  const { checkedItems, paginatedData } = listProps;

  const handleTrack = (
    index: number,
    fn: QualifiedFunction,
    type: AllowedFunctionTypes
  ) => {
    setActiveRow(index);
    setActiveOperation(type);
    trackFunction({
      functionsToBeTracked: [
        {
          function: fn,
          ...(type !== 'root_field'
            ? {
                configuration: {
                  exposed_as: type,
                },
              }
            : {}),
        },
      ],
      onSettled: () => {
        setActiveRow(undefined);
        setActiveOperation(undefined);
      },
    });
  };

  return (
    <div className="space-y-4">
      <TrackableListMenu
        checkActionText={`Track Selected (${checkedItems.length})`}
        isLoading={isLoading}
        {...listProps}
      />
      <CardedTable.Table>
        <CardedTable.TableHead>
          <CardedTable.TableHeadRow>
            <CardedTable.TableHeadCell>Function</CardedTable.TableHeadCell>
            <CardedTable.TableHeadCell>
              <div className="float-right">
                <DropdownMenu
                  items={[
                    [
                      <span
                        className="py-2"
                        onClick={() => invalidateMetadata()}
                      >
                        Refresh
                      </span>,
                    ],
                  ]}
                  options={{
                    content: {
                      alignOffset: -50,
                      avoidCollisions: false,
                    },
                  }}
                >
                  <SlOptionsVertical />
                </DropdownMenu>
              </div>
            </CardedTable.TableHeadCell>
          </CardedTable.TableHeadRow>
        </CardedTable.TableHead>
        <CardedTable.TableBody>
          {paginatedData.map((untrackedFunction, index) => (
            <CardedTable.TableBodyRow>
              <CardedTable.TableBodyCell>
                <FunctionDisplayName
                  qualifiedFunction={untrackedFunction.qualifiedFunction}
                />
              </CardedTable.TableBodyCell>
              <CardedTable.TableBodyCell>
                <div className="flex gap-2 justify-end">
                  {untrackedFunction.isVolatile ? (
                    <>
                      <Button
                        onClick={() => {
                          handleTrack(
                            index,
                            untrackedFunction.qualifiedFunction,
                            'mutation'
                          );
                        }}
                        isLoading={
                          activeRow === index &&
                          isTrackingInProgress &&
                          activeOperation === 'mutation'
                        }
                        disabled={activeRow === index && isTrackingInProgress}
                        loadingText="Please wait..."
                      >
                        Track as Mutation
                      </Button>
                      <Button
                        onClick={() =>
                          hasuraConfirm({
                            message:
                              'Queries are supposed to be read only and as such recommended to be STABLE or IMMUTABLE',
                            title: `Confirm tracking ${untrackedFunction.name} as a query`,
                            onClose: ({ confirmed }) => {
                              if (confirmed)
                                handleTrack(
                                  index,
                                  untrackedFunction.qualifiedFunction,
                                  'query'
                                );
                            },
                          })
                        }
                        isLoading={
                          activeRow === index &&
                          isTrackingInProgress &&
                          activeOperation === 'query'
                        }
                        disabled={activeRow === index && isTrackingInProgress}
                        loadingText="Please wait..."
                      >
                        Track as Query
                      </Button>
                    </>
                  ) : (
                    <Button
                      onClick={() => {
                        handleTrack(
                          index,
                          untrackedFunction.qualifiedFunction,
                          'root_field'
                        );
                      }}
                      isLoading={
                        activeRow === index &&
                        isTrackingInProgress &&
                        activeOperation === 'root_field'
                      }
                      disabled={activeRow === index && isTrackingInProgress}
                      loadingText="Please wait..."
                    >
                      Track as Root Field
                    </Button>
                  )}
                </div>
              </CardedTable.TableBodyCell>
            </CardedTable.TableBodyRow>
          ))}
        </CardedTable.TableBody>
      </CardedTable.Table>
    </div>
  );
};
