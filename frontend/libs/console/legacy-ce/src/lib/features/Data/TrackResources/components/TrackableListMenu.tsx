import React from 'react';
import { Badge } from '../../../../new-components/Badge';
import { Button } from '../../../../new-components/Button';
import { SearchBar } from './SearchBar';
import { PaginatedSearchableListProps } from '../hooks/usePaginatedSearchableList';
import { PageSizeDropdown } from './PageSizeDropdown';

export const TrackableListMenu = (
  props: PaginatedSearchableListProps & {
    isLoading: boolean;
    handleTrackButton?: () => void;
    checkActionText: string;
    showButton?: boolean;
  }
) => (
  <div className="flex justify-between space-x-4">
    <div className="flex gap-5">
      {/* Check Action button */}
      {props.showButton && (
        <>
          <Button
            mode="primary"
            disabled={!props.checkedItems.length}
            onClick={props.handleTrackButton}
            isLoading={props.isLoading}
            loadingText="Please Wait"
          >
            {props.checkActionText}
          </Button>
          <span className="border-r border-slate-300" />
        </>
      )}

      {/* Search Input */}
      <div className="flex gap-2">
        <SearchBar onSearch={props.handleSearch} />
        {props.searchIsActive ? (
          <Badge>{props.filteredData.length} results found</Badge>
        ) : null}
      </div>
    </div>
    <PageSizeDropdown {...props} />
  </div>
);
