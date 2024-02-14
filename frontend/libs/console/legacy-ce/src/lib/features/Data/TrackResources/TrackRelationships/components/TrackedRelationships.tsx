import { useCallback, useMemo } from 'react';
import { TrackedSuggestedRelationship } from '../types';
import { usePaginatedSearchableList } from '../../hooks';
import { useCreateTableRelationships } from '../../../../DatabaseRelationships/hooks/useCreateTableRelationships/useCreateTableRelationships';
import { TrackableListMenu } from '../../components/TrackableListMenu';
import { IndicatorCard } from '../../../../../new-components/IndicatorCard';
import { CardedTable } from '../../../../../new-components/CardedTable';
import { FaMagic } from 'react-icons/fa';
import { TrackedRelationshipRow } from './TrackedRelationshipRow';
import {
  useDestructiveAlert,
  useHasuraAlert,
} from '../../../../../new-components/Alert';
import { hasuraToast } from '../../../../../new-components/Toasts';
import { UNTRACK_RELATIONSHIP_SUCCESS_MESSAGE } from '../constants';
import { DisplayToastErrorMessage } from '../../../components/DisplayErrorMessage';
import React from 'react';
import { anyIncludes, getTableDisplayName } from '../utils';

const getRelationshipRowId = (relationship: TrackedSuggestedRelationship) => {
  const fromTable = getTableDisplayName(relationship.fromTable);
  const toTable = getTableDisplayName(relationship.toTable);

  return `${fromTable}_${toTable}_${relationship.name}`;
};

type TrackedSuggestedRelationshipWithId = TrackedSuggestedRelationship & {
  id: string;
};

export const TrackedSuggestedRelationships = ({
  trackedRelationships,
  dataSourceName,
  onDelete,
  onRename,
  onChange,
}: {
  trackedRelationships: TrackedSuggestedRelationship[];
  dataSourceName: string;
  onDelete?: () => void;
  onRename?: () => void;
  onChange?: () => void;
}) => {
  const listValues: TrackedSuggestedRelationshipWithId[] = useMemo(
    () =>
      trackedRelationships.map(rel => ({
        ...rel,
        id: getRelationshipRowId(rel),
      })),
    [trackedRelationships]
  );

  const { deleteRelationships, renameRelationships, isLoading } =
    useCreateTableRelationships(dataSourceName);

  const filterFn = useCallback(
    (searchText: string, rel: TrackedSuggestedRelationship) =>
      anyIncludes(searchText, [rel.name, rel.type]),
    []
  );

  const listProps = usePaginatedSearchableList<
    TrackedSuggestedRelationship & { id: string }
  >({
    data: listValues,
    filterFn,
  });

  const {
    getCheckedItems,
    checkData: { onCheck, reset, checkedIds, checkAllElement },
    paginatedData: paginatedRelationships,
  } = listProps;

  const { destructiveConfirm } = useDestructiveAlert();
  const { hasuraPrompt } = useHasuraAlert();

  const [loadingIds, setLoadingIds] = React.useState<string[]>([]);

  const onUntrack = (rels: TrackedSuggestedRelationshipWithId[]) => {
    destructiveConfirm({
      resourceName: `${rels.length} relationship(s)`,
      resourceType: 'relationships',
      destroyTerm: 'remove',
      onConfirm: () =>
        new Promise(resolve => {
          setLoadingIds(rels.map(r => r.id));
          deleteRelationships({
            data: rels.map(rel => ({
              name: rel.name,
              source: dataSourceName,
              table: rel.fromTable,
            })),
            onSuccess: () => {
              hasuraToast({
                type: 'success',
                title: UNTRACK_RELATIONSHIP_SUCCESS_MESSAGE,
              });
              resolve(true);
              onDelete?.();
              onChange?.();
            },
            onError: err => {
              hasuraToast({
                type: 'error',
                children: <DisplayToastErrorMessage message={err.message} />,
              });
              resolve(false);
            },
            onSettled: () => {
              setLoadingIds([]);
              reset();
            },
          });
        }),
    });
  };

  const onRenameRelationship = (rel: TrackedSuggestedRelationshipWithId) => {
    hasuraPrompt({
      message:
        'This will change the name of relationship exposed via the GraphQL schema',
      title: 'Rename Relationship',
      confirmText: 'Rename',
      onCloseAsync: async result => {
        if (result.confirmed) {
          await renameRelationships({
            data: [
              {
                name: rel.name,
                source: dataSourceName,
                table: rel.fromTable,
                new_name: result.promptValue,
              },
            ],
          });
          onRename?.();
          onChange?.();
          return { withSuccess: true, successText: 'Saved!' };
        } else {
          return { withSuccess: false };
        }
      },
    });
  };

  return (
    <div className="space-y-4">
      <TrackableListMenu
        checkActionText={`Untrack (${checkedIds.length})`}
        handleTrackButton={() => {
          onUntrack(getCheckedItems());
        }}
        showButton
        isLoading={isLoading}
        {...listProps}
      />
      {paginatedRelationships.length === 0 ? (
        <div className="space-y-4">
          <IndicatorCard>{`No relationships found.`}</IndicatorCard>
        </div>
      ) : (
        <CardedTable.Table>
          <CardedTable.TableHead>
            <CardedTable.TableHeadRow>
              <th className="w-0 bg-gray-50 px-sm text-sm font-semibold text-muted uppercase tracking-wider border-r">
                {checkAllElement()}
              </th>
              <CardedTable.TableHeadCell>
                <div>
                  <FaMagic className="fill-muted" /> SUGGESTED RELATIONSHIPS
                </div>
              </CardedTable.TableHeadCell>
              <CardedTable.TableHeadCell>SOURCE</CardedTable.TableHeadCell>
              <CardedTable.TableHeadCell>TYPE</CardedTable.TableHeadCell>
              <CardedTable.TableHeadCell>
                RELATIONSHIP
              </CardedTable.TableHeadCell>
              <CardedTable.TableHeadCell>ACTIONS</CardedTable.TableHeadCell>
            </CardedTable.TableHeadRow>
          </CardedTable.TableHead>

          <CardedTable.TableBody>
            {paginatedRelationships.map(relationship => (
              <TrackedRelationshipRow
                key={relationship.id}
                isChecked={checkedIds.includes(relationship.id)}
                isLoading={
                  loadingIds.length === 1 && loadingIds[0] === relationship.id
                }
                relationship={relationship}
                onToggle={() => onCheck(relationship.id)}
                onUntrack={() => onUntrack([relationship])}
                onRename={() => onRenameRelationship(relationship)}
                dataSourceName={dataSourceName}
              />
            ))}
          </CardedTable.TableBody>
        </CardedTable.Table>
      )}
    </div>
  );
};
