import { SlOptionsVertical } from 'react-icons/sl';
import Skeleton from 'react-loading-skeleton';
import { Button } from '../../../../../new-components/Button';
import { CardedTable } from '../../../../../new-components/CardedTable';
import { DropdownMenu } from '../../../../../new-components/DropdownMenu';
import { IndicatorCard } from '../../../../../new-components/IndicatorCard';
import { LearnMoreLink } from '../../../../../new-components/LearnMoreLink';
import { IntrospectedFunction, nativeDrivers } from '../../../../DataSource';
import {
  MetadataSelectors,
  useInvalidateMetadata,
  useMetadata,
} from '../../../../hasura-metadata-api';
import { FunctionDisplayName } from './FunctionDisplayName';

import React, { useEffect, useState } from 'react';
import { useHasuraAlert } from '../../../../../new-components/Alert';
import { hasuraToast } from '../../../../../new-components/Toasts';
import { QualifiedFunction } from '../../../../hasura-metadata-types';
import { DisplayToastErrorMessage } from '../../../components/DisplayErrorMessage';
import { useTrackFunction } from '../../../hooks/useTrackFunction';
import { TrackableListMenu } from '../../components/TrackableListMenu';
import { usePaginatedSearchableList } from '../../hooks';
import {
  TrackFunctionForm,
  TrackFunctionFormSchema,
} from './TrackFunctionForm';
import { useInvalidateIntrospectedFunction } from '../../../hooks/useTrackableFunctions';

export type UntrackedFunctionsProps = {
  dataSourceName: string;
  isLoading?: boolean;
  untrackedFunctions: IntrospectedFunction[];
};

export type AllowedFunctionTypes = 'mutation' | 'query' | 'root_field';

export const UntrackedFunctions = (props: UntrackedFunctionsProps) => {
  const { dataSourceName, untrackedFunctions = [], isLoading } = props;

  const functionsWithId = React.useMemo(
    () =>
      Array.isArray(untrackedFunctions)
        ? untrackedFunctions.map(f => ({ ...f, id: f.name }))
        : [],
    [untrackedFunctions]
  );

  const invalidateUntrackedFunctions = useInvalidateIntrospectedFunction();
  const invalidateMetadata = useInvalidateMetadata();

  // Fire this when the component loads because RunSQL could add/remove functions from the database.
  // This is an extra call but it's unavoidable at the moment.
  useEffect(() => {
    invalidateUntrackedFunctions(dataSourceName);
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, []);

  const [activeRow, setActiveRow] = useState<number | undefined>();

  const [isModalOpen, setIsModalOpen] = useState(false);
  const [modalFormDefaultValues, setModalFormDefaultValues] =
    useState<TrackFunctionFormSchema>();

  const [activeOperation, setActiveOperation] =
    useState<AllowedFunctionTypes>();

  const { hasuraConfirm } = useHasuraAlert();

  const { data: driver = '' } = useMetadata(
    m => MetadataSelectors.findSource(dataSourceName)(m)?.kind
  );

  const isNativeDriver = nativeDrivers.includes(driver);

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
    filterFn: (query, item) => {
      return item.name.toLowerCase().includes(query.toLowerCase());
    },
  });

  if (isLoading) return <Skeleton count={5} height={20} className="mb-1" />;

  if (!untrackedFunctions.length)
    return (
      <IndicatorCard status="info" headline="No untracked functions found">
        We couldn't find any compatible functions in your database that can be
        tracked in Hasura.{' '}
        <LearnMoreLink href="https://hasura.io/docs/latest/schema/postgres/postgres-guides/functions/" />
      </IndicatorCard>
    );

  const {
    checkData: { checkedIds },
    paginatedData,
  } = listProps;

  const handleTrack = (
    index: number,
    fn: QualifiedFunction,
    type: AllowedFunctionTypes
  ) => {
    // hack until capabilities or function schema can tell us if the function supports return types
    if (!isNativeDriver) {
      setModalFormDefaultValues({
        qualifiedFunction: JSON.stringify(fn),
        type,
        table: '',
      });
      setIsModalOpen(true);
      return;
    }

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
    <div>
      <div className="space-y-4">
        <TrackableListMenu
          checkActionText={`Track Selected (${checkedIds.length})`}
          isLoading={isLoading ?? false}
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
                          onClick={() => {
                            invalidateUntrackedFunctions(dataSourceName);
                            invalidateMetadata({
                              componentName: 'UntrackedFunctions',
                              reasons: [
                                'Refreshing untracked functions on Dropdown Menu item click.',
                              ],
                            });
                          }}
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
              <CardedTable.TableBodyRow key={untrackedFunction.id}>
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
      <div>
        {isModalOpen ? (
          <TrackFunctionForm
            dataSourceName={dataSourceName}
            onSuccess={() => setIsModalOpen(false)}
            onClose={() => setIsModalOpen(false)}
            defaultValues={modalFormDefaultValues}
          />
        ) : null}
      </div>
    </div>
  );
};
