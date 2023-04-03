import React, { useEffect, useState } from 'react';
import { Button } from '../../../../new-components/Button';
import { CardedTable } from '../../../../new-components/CardedTable';
import { useCheckRows } from '../hooks/useCheckRows';
import {
  DEFAULT_PAGE_NUMBER,
  DEFAULT_PAGE_SIZE,
  DEFAULT_PAGE_SIZES,
} from '../constants';
import { FaAngleLeft, FaAngleRight } from 'react-icons/fa';
import { paginate } from '../utils';
import { SearchBar } from './SearchBar';
import { Badge } from '../../../../new-components/Badge';
import { hasuraToast } from '../../../../new-components/Toasts';
import { TargetName } from '../../../DatabaseRelationships/components/AvailableRelationshipsList/parts/TargetName';
import { RelationshipMapping } from '../../../DatabaseRelationships/components/AvailableRelationshipsList/parts/RelationshipMapping';
import { RowActions } from '../../../DatabaseRelationships/components/AvailableRelationshipsList/parts/RowActions';
import { MODE, Relationship } from '../../../DatabaseRelationships/types';
import { RenderWidget } from '../../../DatabaseRelationships/components/RenderWidget/RenderWidget';
import { NOTIFICATIONS } from '../../../DatabaseRelationships/components/constants';
import { useFireNotification } from '../../../../new-components/Notifications';
import { useMetadataMigration } from '../../../MetadataAPI';
import {
  generateDeleteLocalRelationshipRequest,
  generateRemoteRelationshipDeleteRequest,
} from '../../../DatabaseRelationships/utils/generateRequest';
import { generateQueryKeys } from '../../../DatabaseRelationships/utils/queryClientUtils';
import { useQueryClient } from 'react-query';
import { exportMetadata } from '../../../DataSource';
import { useHttpClient } from '../../../Network';
import { IndicatorCard } from '../../../../new-components/IndicatorCard';
import { MetadataDataSource } from '../../../../metadata/types';
import Skeleton from 'react-loading-skeleton';
import { getSuggestedRelationshipsCacheQuery } from '../../../DatabaseRelationships/components/SuggestedRelationships/hooks/useSuggestedRelationships';

const getQueryFunction = (relationship: Relationship) => {
  if (relationship.type === 'localRelationship') {
    return generateDeleteLocalRelationshipRequest;
  }
  if (
    relationship.type === 'remoteDatabaseRelationship' ||
    relationship.type === 'remoteSchemaRelationship'
  ) {
    return generateRemoteRelationshipDeleteRequest;
  }

  return undefined;
};

const getSerializedRelationshipNames = (relationships: Relationship[]) =>
  relationships.map(rel => rel.name).join('-');

type RelationshipAction = {
  mode?: MODE;
  relationship?: Relationship;
};

interface TrackedRelationshipsProps {
  dataSourceName: string;
  driver?: MetadataDataSource['kind'];
  isLoading: boolean;
  onRefetchMetadata: () => void;
  relationships: Relationship[];
}

export const TrackedRelationships: React.VFC<TrackedRelationshipsProps> = ({
  dataSourceName,
  driver,
  isLoading,
  onRefetchMetadata,
  relationships,
}) => {
  const httpClient = useHttpClient();
  const { mutateAsync } = useMetadataMigration();
  const queryClient = useQueryClient();
  const [isTrackingSelectedRelationships, setTrackingSelectedRelationships] =
    useState(false);

  const [pageNumber, setPageNumber] = useState(DEFAULT_PAGE_NUMBER);
  const [pageSize, setPageSize] = useState(DEFAULT_PAGE_SIZE);
  const [searchText, setSearchText] = useState('');

  const checkboxRef = React.useRef<HTMLInputElement>(null);
  const { checkedIds, onCheck, allChecked, toggleAll, reset, inputStatus } =
    useCheckRows(relationships.map(rel => ({ id: rel.name })));

  useEffect(() => {
    if (!checkboxRef.current) return;
    checkboxRef.current.indeterminate = inputStatus === 'indeterminate';
  }, [inputStatus]);

  const [filteredRelationships, setFilteredRelationships] =
    useState<Relationship[]>(relationships);

  const serializedRelationshipNames =
    getSerializedRelationshipNames(relationships);
  // apply the search text to the relationships
  useEffect(() => {
    reset();

    if (!searchText) {
      setFilteredRelationships(relationships);
      return;
    }

    setFilteredRelationships(
      relationships.filter(rel =>
        rel.name.toLowerCase().includes(searchText.toLowerCase())
      )
    );
  }, [serializedRelationshipNames, searchText]);

  const onUntrackSelected = async () => {
    setTrackingSelectedRelationships(true);
    try {
      const selectedRelationships = relationships.filter(rel =>
        checkedIds.includes(rel.name)
      );

      if (driver) {
        for (let i = 0; i < selectedRelationships.length; i++) {
          const selectedRelationship = selectedRelationships[i];
          const mutationOptions = {
            onSuccess: () => {
              queryClient.invalidateQueries(generateQueryKeys.metadata());

              queryClient.invalidateQueries(
                generateQueryKeys.suggestedRelationships({
                  dataSourceName,
                  table: selectedRelationship.fromTable,
                })
              );
            },
          };

          const queryFunction = getQueryFunction(selectedRelationship);
          if (queryFunction) {
            const recentMetadata = await exportMetadata({ httpClient });
            await mutateAsync(
              {
                query: queryFunction({
                  driver,
                  resource_version: recentMetadata.resource_version,
                  // eslint-disable-next-line @typescript-eslint/ban-ts-comment
                  // @ts-ignore
                  relationship: selectedRelationship,
                }),
              },
              mutationOptions
            );
            queryClient.invalidateQueries(
              getSuggestedRelationshipsCacheQuery(
                dataSourceName,
                selectedRelationship.fromTable
              )
            );
          }
        }

        onRefetchMetadata();

        const relationshipLabel =
          selectedRelationships.length > 1 ? 'Relationships' : 'Relationship';
        const toastMessage = `${selectedRelationships.length} ${relationshipLabel} untracked`;

        hasuraToast({
          title: 'Success',
          message: toastMessage,
          type: 'success',
        });
      }
    } catch (err) {
      setTrackingSelectedRelationships(false);
    }
    reset();
    setTrackingSelectedRelationships(false);
  };

  const { fireNotification } = useFireNotification();
  const [{ mode, relationship }, setRelationshipAction] =
    useState<RelationshipAction>({
      mode: undefined,
      relationship: undefined,
    });

  const onRelationshipActionCancel = () => {
    setRelationshipAction({
      mode: undefined,
      relationship: undefined,
    });
  };

  const onRelationshipActionError = (err: Error) => {
    if (mode)
      fireNotification({
        type: 'error',
        title: NOTIFICATIONS.onError[mode],
        message: err?.message ?? '',
      });
  };

  const onRelationshipActionSuccess = () => {
    if (mode)
      fireNotification({
        type: 'success',
        title: 'Success!',
        message: NOTIFICATIONS.onSuccess[mode],
      });

    setRelationshipAction({
      mode: undefined,
      relationship: undefined,
    });
  };

  if (isLoading) {
    return (
      <div className="px-md">
        <Skeleton count={4} height={25} className="mb-2" />
      </div>
    );
  }

  if (!isLoading && relationships.length === 0) {
    return (
      <div className="space-y-4">
        <IndicatorCard>No tracked relationships found</IndicatorCard>
      </div>
    );
  }

  return (
    <div className="space-y-4">
      <div className="flex justify-between space-x-4">
        <div className="flex gap-5">
          <Button
            mode="primary"
            disabled={!checkedIds.length}
            onClick={onUntrackSelected}
            isLoading={isTrackingSelectedRelationships}
            loadingText="Please Wait"
          >
            Untrack Selected ({checkedIds.length})
          </Button>

          <span className="border-r border-slate-300"></span>

          <div className="flex gap-2">
            <SearchBar onSearch={data => setSearchText(data)} />
            {searchText.length ? (
              <Badge>{filteredRelationships.length} results found</Badge>
            ) : null}
          </div>
        </div>

        <div className="flex gap-1">
          <Button
            icon={<FaAngleLeft />}
            onClick={() => setPageNumber(pageNumber - 1)}
            disabled={pageNumber === 1}
          />
          <select
            value={pageSize}
            onChange={e => {
              setPageSize(Number(e.target.value));
            }}
            className="block w-full max-w-xl h-8 min-h-full shadow-sm rounded pl-3 pr-6 py-0.5 border border-gray-300 hover:border-gray-400 focus-visible:outline-0 focus-visible:ring-2 focus-visible:ring-yellow-200 focus-visible:border-yellow-400"
          >
            {DEFAULT_PAGE_SIZES.map(_pageSize => (
              <option key={_pageSize} value={_pageSize}>
                Show {_pageSize} relationships
              </option>
            ))}
          </select>
          <Button
            icon={<FaAngleRight />}
            onClick={() => setPageNumber(pageNumber + 1)}
            disabled={pageNumber >= relationships.length / pageSize}
          />
        </div>
      </div>

      <CardedTable.Table>
        <CardedTable.TableHead>
          <CardedTable.TableHeadRow>
            <th className="w-0 bg-gray-50 px-sm text-sm font-semibold text-muted uppercase tracking-wider border-r">
              <input
                ref={checkboxRef}
                type="checkbox"
                className="cursor-pointer
                  rounded border shadow-sm border-gray-400 hover:border-gray-500 focus:ring-yellow-400"
                checked={allChecked}
                onChange={toggleAll}
              />
            </th>
            <CardedTable.TableHeadCell>
              RELATIONSHIP NAME
            </CardedTable.TableHeadCell>
            <CardedTable.TableHeadCell>SOURCE</CardedTable.TableHeadCell>
            <CardedTable.TableHeadCell>TYPE</CardedTable.TableHeadCell>
            <CardedTable.TableHeadCell>RELATIONSHIP</CardedTable.TableHeadCell>
            <CardedTable.TableHeadCell></CardedTable.TableHeadCell>
          </CardedTable.TableHeadRow>
        </CardedTable.TableHead>

        <CardedTable.TableBody>
          {paginate(filteredRelationships, pageSize, pageNumber).map(
            relationship => {
              return (
                <CardedTable.TableBodyRow key={relationship.name}>
                  <td className="w-0 px-sm text-sm font-semibold text-muted uppercase tracking-wider">
                    <input
                      type="checkbox"
                      className="cursor-pointer rounded border shadow-sm border-gray-400 hover:border-gray-500 focus:ring-yellow-400"
                      value={relationship.name}
                      checked={checkedIds.includes(relationship.name)}
                      onChange={() => onCheck(relationship.name)}
                    />
                  </td>
                  <CardedTable.TableBodyCell>
                    {relationship.name}
                  </CardedTable.TableBodyCell>
                  <CardedTable.TableBodyCell>
                    <div className="flex items-center gap-2">
                      <TargetName relationship={relationship} />
                    </div>
                  </CardedTable.TableBodyCell>
                  <CardedTable.TableBodyCell>
                    {relationship.relationshipType}
                  </CardedTable.TableBodyCell>

                  <CardedTable.TableBodyCell>
                    <RelationshipMapping relationship={relationship} />
                  </CardedTable.TableBodyCell>

                  <CardedTable.TableBodyActionCell>
                    <RowActions
                      relationship={relationship}
                      onActionClick={(_relationship, _mode) => {
                        setRelationshipAction({
                          mode: _mode,
                          relationship: _relationship,
                        });
                      }}
                    />
                  </CardedTable.TableBodyActionCell>
                </CardedTable.TableBodyRow>
              );
            }
          )}
        </CardedTable.TableBody>
      </CardedTable.Table>
      <div>
        {mode && (
          <RenderWidget
            dataSourceName={dataSourceName}
            table={relationship?.fromTable}
            mode={mode}
            relationship={relationship}
            onSuccess={onRelationshipActionSuccess}
            onCancel={onRelationshipActionCancel}
            onError={onRelationshipActionError}
          />
        )}
      </div>
    </div>
  );
};
