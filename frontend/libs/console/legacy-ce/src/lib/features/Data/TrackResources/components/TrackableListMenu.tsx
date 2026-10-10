import React from 'react';
import { Badge, Button, SearchInput } from '@hasura/shared/ui';
import { Flex } from '@radix-ui/themes';
import { PaginatedSearchableListProps } from '../hooks/usePaginatedSearchableList';
import { PageSizeDropdown } from './PageSizeDropdown';

export const TrackableListMenu = (
  props: PaginatedSearchableListProps & {
    isLoading: boolean;
    handleTrackButton?: () => void;
    checkActionText: string;
    showButton?: boolean;
    searchChildren?: React.ReactNode;
    actionChildren?: React.ReactNode;
  },
) => (
  <Flex justify="between" className="space-x-4">
    <Flex gap="5">
      {/* Check Action button */}
      {props.showButton && (
        <>
          <Button
            mode="primary"
            disabled={!props.checkData.checkedIds.length}
            onClick={props.handleTrackButton}
            loading={props.isLoading}
            loadingText="Please Wait"
          >
            {props.checkActionText}
          </Button>
          {props.actionChildren && props.actionChildren}
          <span className="border-r border-slate-300" />
        </>
      )}

      {/* Search Input */}
      <Flex gap="2" align="center">
        <SearchInput onSearch={props.handleSearch} />
        {props.searchChildren && props.searchChildren}
        {props.searchIsActive ? (
          <Badge>{props.filteredData.length} results found</Badge>
        ) : null}
      </Flex>
    </Flex>
    <PageSizeDropdown {...props} />
  </Flex>
);
