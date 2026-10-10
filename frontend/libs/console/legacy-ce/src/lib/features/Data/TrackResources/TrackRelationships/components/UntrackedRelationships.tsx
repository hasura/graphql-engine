import React, { useCallback } from 'react';
import {
  useHasuraAlert,
  CardedTable,
  IndicatorCard,
  hasuraToast,
  DisplayToastErrorMessage,
  Checkbox,
  Button,
} from '@hasura/shared/ui';
import { TrackableListMenu } from '../../components/TrackableListMenu';
import { usePaginatedSearchableList } from '../../hooks';
import { capitalize } from 'inflection';
import { anyIncludes } from '../utils';
import { SuggestedRelationshipWithName } from '@hasura/metadata/api';
import { useCreateTableRelationships } from '@hasura/metadata/data-source';
import { getTableHeaderRow } from './utils';
import { Flex } from '@radix-ui/themes';
import { FaDatabase } from 'react-icons/fa6';
import { capitalizeFirstLetter } from '@hasura/shared/utils';
import { DisplaySuggestedRelationship } from '../../../../DatabaseRelationships/components/common/mapping/DisplaySuggestedRelationship';

export const UntrackedRelationships = ({
  untrackedRelationships,
  dataSourceName,
  onTrack,
}: {
  untrackedRelationships: SuggestedRelationshipWithName[];
  dataSourceName: string;
  onTrack?: () => void;
}) => {
  const filterFn = useCallback(
    (searchText: string, rel: SuggestedRelationshipWithName) =>
      anyIncludes(searchText, [rel.constraintName, rel.type]),
    [],
  );

  const listProps = usePaginatedSearchableList<SuggestedRelationshipWithName>({
    data: untrackedRelationships,
    filterFn,
  });

  const {
    getCheckedItems,
    checkData: { reset, onCheck, checkedIds, checkAllElement },
    paginatedData: paginatedRelationships,
  } = listProps;

  const { createTableRelationships, isPending } =
    useCreateTableRelationships(dataSourceName);

  const [loadingIds, setLoadingIds] = React.useState<string[]>([]);

  const onTrackRelationships = async (
    relationships: SuggestedRelationshipWithName[],
  ): Promise<boolean> =>
    new Promise((resolve) => {
      setLoadingIds(relationships.map((r) => r.id));
      createTableRelationships(
        relationships.map((rel) => ({
          name: rel.constraintName,
          source: {
            fromSource: dataSourceName,
            fromTable: rel.from.table,
          },
          definition: {
            target: {
              toSource: dataSourceName,
              toTable: rel.to.table,
            },
            type: rel.type,
            detail: {
              fkConstraintOn:
                'constraint_name' in rel.from ? 'fromTable' : 'toTable',
              fromColumns: rel.from.columns,
              toColumns: rel.to.columns,
            },
          },
        })),
        {
          onSettled: () => {
            reset();
            setLoadingIds([]);
          },
          onSuccess: () => {
            hasuraToast({
              type: 'success',
              title: 'Successfully tracked relationships',
            });
            resolve(true);
            onTrack?.();
          },
          onError: (err) => {
            hasuraToast({
              type: 'error',
              title: 'Error while tracking relationships',
              children: <DisplayToastErrorMessage message={err.message} />,
            });
            resolve(false);
          },
        },
      );
    });

  const { hasuraPrompt } = useHasuraAlert();

  const onCustomize = (relationship: SuggestedRelationshipWithName) => {
    hasuraPrompt({
      title: `Track ${capitalize(relationship.type)} relationship:`,
      message: (
        <div className="py-4">
          <DisplaySuggestedRelationship relationship={relationship} />
        </div>
      ),
      sanitizeGraphQL: true,
      confirmText: 'Add Relationship',
      defaultValue: relationship.constraintName,
      inputFieldName: 'Relationship Name',
      onCloseAsync: (response) =>
        new Promise((resolve) => {
          if (response.confirmed) {
            onTrackRelationships([
              {
                ...relationship,
                constraintName: response.promptValue,
              },
            ]).then((success) => {
              resolve({
                withSuccess: success,
                successText: 'Added!',
              });
              onTrack?.();
            });
          } else {
            return resolve({ withSuccess: false });
          }
        }),
    });
  };

  return (
    <div className="space-y-4">
      <TrackableListMenu
        checkActionText={`Track (${checkedIds.length})`}
        handleTrackButton={() => {
          onTrackRelationships(getCheckedItems());
        }}
        showButton
        isLoading={isPending}
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
                key={`${relationship.id}-select`}
                value={checkedIds.includes(relationship.id)}
                onChange={() => onCheck(relationship.id)}
              />,
              relationship.constraintName,
              <Flex key={`${relationship.id}-source`} align="center" gap="2">
                <FaDatabase /> <span>{dataSourceName}</span>
              </Flex>,
              capitalizeFirstLetter(relationship.type),
              <DisplaySuggestedRelationship
                key={`${relationship.id}-suggested`}
                relationship={relationship}
              />,
              <Flex
                key={`${relationship.id}-actions`}
                direction="row"
                gap="2"
                align="center"
              >
                <Button
                  mode="primary"
                  size="sm"
                  onClick={() => onTrackRelationships([relationship])}
                  disabled={isLoading}
                >
                  Track
                </Button>
                <Button
                  size="sm"
                  mode="default"
                  onClick={() => onCustomize(relationship)}
                  disabled={isLoading}
                >
                  Customize
                </Button>
              </Flex>,
            ];
          })}
        />
      )}
    </div>
  );
};
