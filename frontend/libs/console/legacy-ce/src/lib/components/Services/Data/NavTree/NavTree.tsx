import { Input } from '../../../../new-components/Form';
import { useState } from 'react';
import { FaSearch } from 'react-icons/fa';
import {
  useInconsistentMetadata,
  useMetadata,
} from '../../../../features/hasura-metadata-api';
import Skeleton from 'react-loading-skeleton';
import {
  QualifiedFunction,
  Table,
} from '../../../../features/hasura-metadata-types';
import { useAvailableDrivers } from '../../../../features/ConnectDB';
import { TreeComponent } from './components/TreeComponent';
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

type Props = {
  defaultSelection?: Selection;
  handleTableClick?: (table: Table) => void;
  handleDatabaseClick?: (dataSourceName: string) => void;
};

export const NavTree = (props: Props) => {
  const [term, setTerm] = useState('');

  const selection = JSON.stringify(props?.defaultSelection ?? {});

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

  const { data: treeData = [], isLoading } = useMetadata(
    m => adaptSourcesIntoTreeData(m)(drivers, inconsistentData),
    {
      enabled: !isFetching && !isDriverInfoFetching,
    }
  );

  if (isLoading) return <Skeleton height={30} count={8} />;

  console.log(treeData, selection);

  return (
    <div id="tree-stuff">
      <div>
        <Input
          name="search"
          onChange={e => setTerm(e.target.value)}
          icon={<FaSearch />}
        />
      </div>
      <TreeComponent
        treeData={treeData}
        onDatabaseClick={data => console.log(data)}
        onLeafNodeClick={data => console.log(data)}
        selection={selection}
        term={term}
      />
    </div>
  );
};
