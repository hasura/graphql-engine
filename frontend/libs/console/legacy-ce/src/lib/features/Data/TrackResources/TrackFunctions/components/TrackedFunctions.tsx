import { SlOptionsVertical } from 'react-icons/sl';
import Skeleton from 'react-loading-skeleton';
import { Button } from '../../../../../new-components/Button';
import { CardedTable } from '../../../../../new-components/CardedTable';
import { DropdownMenu } from '../../../../../new-components/DropdownMenu';
import { IndicatorCard } from '../../../../../new-components/IndicatorCard';
import { LearnMoreLink } from '../../../../../new-components/LearnMoreLink';
import { useInvalidateMetadata } from '../../../../hasura-metadata-api';
import { FunctionDisplayName } from './FunctionDisplayName';
import React, { useState } from 'react';
import { TrackableListMenu } from '../../components/TrackableListMenu';
import { usePaginatedSearchableList } from '../../hooks';
import { hasuraToast } from '../../../../../new-components/Toasts';
import { DisplayToastErrorMessage } from '../../../components/DisplayErrorMessage';
import { useTrackFunction } from '../../../hooks/useTrackFunction';
import { ModifyFunctionConfiguration } from '../../../ManageFunction/components/ModifyFunctionConfiguration';
import { FaEdit } from 'react-icons/fa';

export type TrackedFunctionsProps = {
  dataSourceName: string;
  trackedFunctions: {
    qualifiedFunction: unknown;
    name: string;
  }[];
  isLoading?: boolean;
};

export const TrackedFunctions = (props: TrackedFunctionsProps) => {
  const { dataSourceName, trackedFunctions, isLoading } = props;

  const [isConfigurationModalOpen, setIsConfigurationModalOpen] =
    useState(false);

  const [activeRow, setActiveRow] = useState<number | undefined>();

  const functionsWithId = React.useMemo(() => {
    if (Array.isArray(trackedFunctions)) {
      return trackedFunctions.map(f => ({ ...f, id: f.name }));
    } else {
      return [];
    }
  }, [trackedFunctions]);

  const invalidateMetadata = useInvalidateMetadata();

  const { untrackFunction, isLoading: isUntrackingInProgress } =
    useTrackFunction({
      dataSourceName,
      onError: err => {
        hasuraToast({
          type: 'error',
          title: err.name,
          children: <DisplayToastErrorMessage message={err.message} />,
        });
      },
    });

  const searchFn = React.useCallback((query, item) => {
    return item.name.toLowerCase().includes(query.toLowerCase());
  }, []);

  const listProps = usePaginatedSearchableList({
    data: functionsWithId,
    filterFn: searchFn,
  });

  const {
    checkData: { onCheck, checkedIds, reset, checkAllElement },
    paginatedData,
    getCheckedItems,
  } = listProps;

  if (isLoading) return <Skeleton count={5} height={20} className="mb-1" />;

  if (!trackedFunctions.length)
    return (
      <IndicatorCard status="info" headline="No untracked functions found">
        We couldn't find any tracked functions in your metadata.{' '}
        <LearnMoreLink href="https://hasura.io/docs/latest/schema/postgres/postgres-guides/functions/" />
      </IndicatorCard>
    );

  return (
    <div className="space-y-4">
      <TrackableListMenu
        checkActionText={`Untrack Selected (${checkedIds.length})`}
        handleTrackButton={() => {
          untrackFunction({
            functionsToBeUntracked: getCheckedItems().map(
              fn => fn.qualifiedFunction
            ),
            onSuccess: () => {
              hasuraToast({
                type: 'success',
                title: 'Success',
                message: `Untracked ${checkedIds.length} objects`,
              });
              reset();
            },
          });
        }}
        isLoading={isUntrackingInProgress}
        {...listProps}
        showButton
      />
      <CardedTable.Table>
        <CardedTable.TableHead>
          <CardedTable.TableHeadRow>
            <th className="w-0 bg-gray-50 px-sm text-sm font-semibold text-muted uppercase tracking-wider border-r">
              {checkAllElement()}
            </th>
            <CardedTable.TableHeadCell>Function</CardedTable.TableHeadCell>
            <CardedTable.TableHeadCell>
              <div className="float-right">
                <DropdownMenu
                  items={[
                    [
                      <span
                        className="py-2"
                        onClick={() =>
                          invalidateMetadata({
                            componentName: 'TrackedFunctions',
                            reasons: [
                              'Refreshing tracked functions on dropdown menu item click.',
                            ],
                          })
                        }
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
          {paginatedData.map((trackedFunction, index) => (
            <CardedTable.TableBodyRow>
              <td className="w-0 px-sm text-sm font-semibold text-muted uppercase tracking-wider">
                <input
                  type="checkbox"
                  className="cursor-pointer rounded border shadow-sm border-gray-400 hover:border-gray-500 focus:ring-yellow-400"
                  value={trackedFunction.name}
                  checked={checkedIds.includes(trackedFunction.name)}
                  onChange={() => onCheck(trackedFunction.name)}
                />
              </td>
              <CardedTable.TableBodyCell>
                <FunctionDisplayName
                  qualifiedFunction={trackedFunction.qualifiedFunction}
                />
              </CardedTable.TableBodyCell>
              <CardedTable.TableBodyCell>
                <div className="flex gap-2 justify-end">
                  {isConfigurationModalOpen ? (
                    <ModifyFunctionConfiguration
                      dataSourceName={dataSourceName}
                      qualifiedFunction={trackedFunction.qualifiedFunction}
                      onSuccess={() => setIsConfigurationModalOpen(false)}
                      onClose={() => setIsConfigurationModalOpen(false)}
                    />
                  ) : null}
                  <Button
                    onClick={() => {
                      setIsConfigurationModalOpen(true);
                    }}
                    icon={<FaEdit />}
                  >
                    Configure
                  </Button>
                  <Button
                    onClick={() => {
                      setActiveRow(index);
                      untrackFunction({
                        functionsToBeUntracked: [
                          trackedFunction.qualifiedFunction,
                        ],
                        onSuccess: () => {
                          hasuraToast({
                            type: 'success',
                            title: 'Success',
                            message: `Untracked object`,
                          });
                          setActiveRow(undefined);
                        },
                      });
                    }}
                    isLoading={activeRow === index && isUntrackingInProgress}
                    loadingText="Please wait"
                  >
                    Untrack
                  </Button>
                </div>
              </CardedTable.TableBodyCell>
            </CardedTable.TableBodyRow>
          ))}
        </CardedTable.TableBody>
      </CardedTable.Table>
    </div>
  );
};
