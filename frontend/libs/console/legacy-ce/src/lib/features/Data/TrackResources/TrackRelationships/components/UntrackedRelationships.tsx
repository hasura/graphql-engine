import React, { useCallback } from 'react';
import { FaMagic } from 'react-icons/fa';
import { useHasuraAlert } from '../../../../../new-components/Alert';
import { CardedTable } from '../../../../../new-components/CardedTable';
import { IndicatorCard } from '../../../../../new-components/IndicatorCard';
import { hasuraToast } from '../../../../../new-components/Toasts';
import { useCreateTableRelationships } from '../../../../DatabaseRelationships/hooks/useCreateTableRelationships/useCreateTableRelationships';
import { DisplayToastErrorMessage } from '../../../components/DisplayErrorMessage';
import { TrackableListMenu } from '../../components/TrackableListMenu';
import { usePaginatedSearchableList } from '../../hooks';
import {
  DisplaySuggestedRelationship,
  UntrackedRelationshipRow,
} from './UntrackedRelationshipRow';
import { SuggestedRelationshipWithName } from '../types';
import capitalize from 'lodash/capitalize';
import { anyIncludes } from '../utils';

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
    []
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

  const { createTableRelationships, isLoading } =
    useCreateTableRelationships(dataSourceName);

  const [loadingIds, setLoadingIds] = React.useState<string[]>([]);

  const onTrackRelationships = async (
    relationships: SuggestedRelationshipWithName[]
  ): Promise<boolean> =>
    new Promise(resolve => {
      setLoadingIds(relationships.map(r => r.id));
      createTableRelationships({
        data: relationships.map(rel => ({
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
        onError: err => {
          hasuraToast({
            type: 'error',
            title: 'Error while tracking relationships',
            children: <DisplayToastErrorMessage message={err.message} />,
          });
          resolve(false);
        },
      });
    });

  const { hasuraPrompt } = useHasuraAlert();

  const onCustomize = (relationship: SuggestedRelationshipWithName) => {
    hasuraPrompt({
      title: `Track ${capitalize(relationship.type)} relationship:`,
      message: (
        <div>
          <DisplaySuggestedRelationship relationship={relationship} />
        </div>
      ),
      sanitizeGraphQL: true,
      confirmText: 'Add Relationship',
      defaultValue: relationship.constraintName,
      inputFieldName: 'Relationship Name',
      onCloseAsync: response =>
        new Promise(resolve => {
          if (response.confirmed) {
            onTrackRelationships([
              {
                ...relationship,
                constraintName: response.promptValue,
              },
            ]).then(success => {
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
              <UntrackedRelationshipRow
                key={relationship.id}
                isChecked={checkedIds.includes(relationship.id)}
                // we only want this row to show it's own loading state if we are tracking a single item and it's this one
                isLoading={
                  loadingIds.length === 1 && loadingIds[0] === relationship.id
                }
                relationship={relationship}
                onToggle={() => onCheck(relationship.id)}
                onTrack={() => onTrackRelationships([relationship])}
                onCustomize={() => onCustomize(relationship)}
                dataSourceName={dataSourceName}
              />
            ))}
          </CardedTable.TableBody>
        </CardedTable.Table>
      )}
    </div>
  );
};
