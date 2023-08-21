import React, { useState } from 'react';
import { FaSearch } from 'react-icons/fa';
import { Input } from '../../../new-components/Form';
import { useAvailableDrivers } from '../../ConnectDB';
import {
  useInconsistentMetadata,
  useMetadata,
} from '../../hasura-metadata-api';
import { QualifiedFunction, Table } from '../../hasura-metadata-types';
import { DATABASE_SEARCH_INPUT_ID } from '../constants';
import { LeafType, TreeComponent } from './components/TreeComponent';
import {
  adaptInconsistentObjects,
  adaptSourcesIntoTreeData,
} from './selectors';

type Selection = {
  dataSourceName: string;
} & (
  | {
      table: Table;
      function?: never;
    }
  | {
      table?: never;
      function: QualifiedFunction;
    }
);

type NavTreeProps = {
  defaultSelection?: Selection;

  handleTableClick?: (tableDetails: LeafType) => void;
  handleDatabaseClick?: (dataSourceName: string) => void;
};

const twInputStyles = `border-none shadow-none focus-visible:ring-0 p-3 text-[20px] h-auto`;

export const NavTree = ({
  handleDatabaseClick,
  handleTableClick,
  defaultSelection,
}: NavTreeProps) => {
  const [term, setTerm] = useState('');

  const selection = JSON.stringify(defaultSelection ?? {});

  // I'm not sure why we need to do this, why can't we just loop over sources in metadata?
  const { data: drivers = [], isFetching: isDriverInfoFetching } =
    useAvailableDrivers();

  const {
    data: inconsistentData = {
      inconsistentSources: [],
      inconsistentTables: [],
      inconsistentFunctions: [],
    },
    isFetching,
  } = useInconsistentMetadata(adaptInconsistentObjects);

  const metadataSelector = React.useCallback(
    m => adaptSourcesIntoTreeData(m)(drivers, inconsistentData),
    [drivers, inconsistentData]
  );

  const { data: treeData = [], isLoading } = useMetadata(metadataSelector, {
    enabled: !isFetching && !isDriverInfoFetching,
  });

  if (isLoading) return null;

  return (
    <div id="tree-stuff">
      <Input
        name={DATABASE_SEARCH_INPUT_ID}
        onChange={e => setTerm(e.target.value)}
        icon={<FaSearch />}
        inputClassName={twInputStyles}
        className="border-b"
      />
      <TreeComponent
        treeData={treeData}
        onDatabaseClick={data => handleDatabaseClick?.(data)}
        onLeafNodeClick={data => handleTableClick?.(data)}
        selection={selection}
        term={term}
      />
    </div>
  );
};
