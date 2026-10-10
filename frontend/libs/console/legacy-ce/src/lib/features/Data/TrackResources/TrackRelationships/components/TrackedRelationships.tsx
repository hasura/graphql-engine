import React, { useCallback, useMemo } from 'react';
import { usePaginatedSearchableList } from '../../hooks';
import { TrackableListMenu } from '../../components/TrackableListMenu';
import {
  IndicatorCard,
  CardedTable,
  useDestructiveAlert,
  useHasuraAlert,
  hasuraToast,
  DisplayToastErrorMessage,
  Checkbox,
  Button,
  Text,
} from '@hasura/shared/ui';
import DisplaySuggestedRelationship from './DisplaySuggestedRelationship';
import { UNTRACK_RELATIONSHIP_SUCCESS_MESSAGE } from '../constants';
import { anyIncludes } from '../utils';
import {
  TrackedSuggestedRelationship,
  useDropRelationships,
  useRenameRelationship,
} from '@hasura/metadata/api';
import { Flex } from '@radix-ui/themes';
import { FaDatabase } from 'react-icons/fa6';
import { getTableHeaderRow } from './utils';
import {
  capitalizeFirstLetter,
  getTableDisplayName,
} from '@hasura/shared/utils';

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
      trackedRelationships.map((rel) => ({
        ...rel,
        id: getRelationshipRowId(rel),
      })),
    [trackedRelationships],
  );

  const { renameRelationship, isPending: renameLoading } =
    useRenameRelationship();
  const { dropRelationships, isPending: dropLoading } = useDropRelationships();

  const filterFn = useCallback(
    (searchText: string, rel: TrackedSuggestedRelationship) =>
      anyIncludes(searchText, [rel.name, rel.type]),
    [],
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
        new Promise((resolve) => {
          setLoadingIds(rels.map((r) => r.id));
          dropRelationships(
            rels.map((rel) => ({
              relationship: rel.name,
              source: dataSourceName,
              table: rel.fromTable,
            })),
            {
              onSuccess: () => {
                hasuraToast({
                  type: 'success',
                  title: UNTRACK_RELATIONSHIP_SUCCESS_MESSAGE,
                });
                resolve(true);
                onDelete?.();
                onChange?.();
              },
              onError: (err) => {
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
            },
          );
        }),
    });
  };

  const onRenameRelationship = (rel: TrackedSuggestedRelationshipWithId) => {
    hasuraPrompt({
      message: (
        <div className="mb-2">
          <Text>
            &apos;This will change the name of relationship exposed via the
            GraphQL schema&apos;
          </Text>
        </div>
      ),
      title: 'Rename Relationship',
      confirmText: 'Rename',
      onCloseAsync: async (result) => {
        if (result.confirmed) {
          await renameRelationship({
            name: rel.name,
            source: dataSourceName,
            table: rel.fromTable,
            new_name: result.promptValue,
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

  const isLoading = renameLoading || dropLoading;

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
        <CardedTable
          columns={getTableHeaderRow(checkAllElement())}
          data={paginatedRelationships.map((relationship) => {
            const isLoading =
              loadingIds.length === 1 && loadingIds[0] === relationship.id;

            return [
              <Checkbox
                key={`check-${relationship.id}`}
                value={checkedIds.includes(relationship.id)}
                onChange={() => onCheck(relationship.id)}
              />,
              relationship.name,
              <Flex key={`source-${relationship.id}`} align="center" gap="2">
                <FaDatabase /> <span>{dataSourceName}</span>
              </Flex>,
              capitalizeFirstLetter(relationship.type),
              <DisplaySuggestedRelationship
                key={`rel-${relationship.id}`}
                relationship={relationship}
              />,
              <Flex key={`actions-${relationship.id}`} align="center" gap="2">
                <Button
                  mode="destructive"
                  size="sm"
                  onClick={() => onUntrack([relationship])}
                  disabled={isLoading}
                >
                  Untrack
                </Button>
                <Button
                  size="sm"
                  className="ml-1"
                  onClick={() => onRenameRelationship(relationship)}
                  disabled={isLoading}
                >
                  Rename
                </Button>
              </Flex>,
            ];
          })}
        />
      )}
    </div>
  );
};
