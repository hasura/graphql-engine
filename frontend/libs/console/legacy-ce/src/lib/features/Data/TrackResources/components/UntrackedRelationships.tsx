import React, { useEffect, useState } from 'react';

import { Button } from '../../../../new-components/Button';
import { CardedTable } from '../../../../new-components/CardedTable';
import { IndicatorCard } from '../../../../new-components/IndicatorCard';
import { DEFAULT_PAGE_SIZES } from '../constants';
import { FaAngleLeft, FaAngleRight, FaMagic } from 'react-icons/fa';
import { DEFAULT_PAGE_NUMBER, DEFAULT_PAGE_SIZE } from '../constants';
import { paginate } from '../utils';
import { SearchBar } from './SearchBar';
import { Badge } from '../../../../new-components/Badge';
import { SuggestedRelationshipWithName } from '../../../DatabaseRelationships/components/SuggestedRelationships/hooks/useSuggestedRelationships';
import { RelationshipRow } from './RelationshipRow';
import { SuggestedRelationshipTrackModal } from '../../../DatabaseRelationships/components/SuggestedRelationshipTrackModal/SuggestedRelationshipTrackModal';
import Skeleton from 'react-loading-skeleton';
import { useAllSuggestedRelationships } from '../../../DatabaseRelationships/components/SuggestedRelationships/hooks/useAllSuggestedRelationships';
import { useCheckRows } from '../../../DatabaseRelationships/hooks/useCheckRows';
import { useTrackedRelationships } from './hooks/useTrackedRelationships';

interface UntrackedRelationshipsProps {
  dataSourceName: string;
}

export const UntrackedRelationships: React.VFC<UntrackedRelationshipsProps> = ({
  dataSourceName,
}) => {
  const [pageNumber, setPageNumber] = useState(DEFAULT_PAGE_NUMBER);
  const [pageSize, setPageSize] = useState(DEFAULT_PAGE_SIZE);
  const [searchText, setSearchText] = useState('');

  const [isModalVisible, setModalVisible] = useState(false);
  const [selectedRelationship, setSelectedRelationship] =
    useState<SuggestedRelationshipWithName | null>(null);

  const {
    suggestedRelationships,
    isLoadingSuggestedRelationships,
    onAddMultipleSuggestedRelationships,
  } = useAllSuggestedRelationships({
    dataSourceName,
    isEnabled: true,
    omitTracked: true,
  });

  const { data: trackedRelationships } =
    useTrackedRelationships(dataSourceName);

  const checkboxRef = React.useRef<HTMLInputElement>(null);
  const { checkedIds, onCheck, allChecked, toggleAll, reset, inputStatus } =
    useCheckRows(
      suggestedRelationships.map(rel => ({ id: rel.constraintName }))
    );

  const [filteredRelationships, setFilteredRelationships] = useState<
    SuggestedRelationshipWithName[]
  >(suggestedRelationships);
  console.log('trackedRelationships', trackedRelationships);
  console.log('suggestedRelationships', suggestedRelationships);
  const serializedRelationshipNames = suggestedRelationships
    .map(rel => rel.constraintName)
    .join('-');
  useEffect(() => {
    reset();
    if (!searchText) {
      setFilteredRelationships(suggestedRelationships);
      return;
    }

    setFilteredRelationships(
      suggestedRelationships.filter(rel =>
        rel.constraintName.toLowerCase().includes(searchText.toLowerCase())
      )
    );
  }, [serializedRelationshipNames, searchText]);

  useEffect(() => {
    if (!checkboxRef.current) return;
    checkboxRef.current.indeterminate = inputStatus === 'indeterminate';
  }, [inputStatus]);

  const onTrackRelationship = async (
    relationship: SuggestedRelationshipWithName
  ) => {
    await onAddMultipleSuggestedRelationships([relationship]);
  };

  const [isTrackingSelectedRelationships, setTrackingSelectedRelationships] =
    useState(false);
  const onTrackSelected = async () => {
    setTrackingSelectedRelationships(true);
    try {
      const selectedRelationships = suggestedRelationships.filter(rel =>
        checkedIds.includes(rel.constraintName)
      );

      await onAddMultipleSuggestedRelationships(selectedRelationships);
    } catch (err) {
      setTrackingSelectedRelationships(false);
    }
    reset();
    setTrackingSelectedRelationships(false);
  };

  if (isLoadingSuggestedRelationships) {
    return (
      <div className="px-sm -mt-2 mb-xs">
        <Skeleton width={200} height={20} />
      </div>
    );
  }

  if (!isLoadingSuggestedRelationships && suggestedRelationships.length === 0) {
    return (
      <div className="space-y-4">
        <IndicatorCard>No untracked relationships found</IndicatorCard>
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
            onClick={onTrackSelected}
            isLoading={isTrackingSelectedRelationships}
            loadingText="Please Wait"
          >
            Track Selected ({checkedIds.length})
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
            disabled={pageNumber >= suggestedRelationships.length / pageSize}
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
              <div>
                <FaMagic className="fill-muted" /> SUGGESTED RELATIONSHIPS
              </div>
            </CardedTable.TableHeadCell>
            <CardedTable.TableHeadCell>SOURCE</CardedTable.TableHeadCell>
            <CardedTable.TableHeadCell>TYPE</CardedTable.TableHeadCell>
            <CardedTable.TableHeadCell>RELATIONSHIP</CardedTable.TableHeadCell>
            <CardedTable.TableHeadCell>ACTIONS</CardedTable.TableHeadCell>
          </CardedTable.TableHeadRow>
        </CardedTable.TableHead>

        <CardedTable.TableBody>
          {paginate({
            data: filteredRelationships,
            pageSize,
            pageNumber,
          }).data.map(relationship => (
            <RelationshipRow
              key={relationship.constraintName}
              isChecked={checkedIds.includes(relationship.constraintName)}
              isLoading={false}
              relationship={relationship}
              onToggle={() => onCheck(relationship.constraintName)}
              onTrack={() => onTrackRelationship(relationship)}
              onCustomize={() => {
                setSelectedRelationship(relationship);
                setModalVisible(true);
              }}
              dataSourceName={dataSourceName}
            />
          ))}
        </CardedTable.TableBody>
      </CardedTable.Table>
      {isModalVisible && selectedRelationship && (
        <SuggestedRelationshipTrackModal
          relationship={selectedRelationship}
          dataSourceName={dataSourceName}
          onClose={() => setModalVisible(false)}
        />
      )}
    </div>
  );
};
