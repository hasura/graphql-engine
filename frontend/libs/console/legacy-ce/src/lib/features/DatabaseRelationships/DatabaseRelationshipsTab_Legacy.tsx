import { RightContainer } from '../../components/Common/Layout/RightContainer';
import { Driver } from '../../dataSources';
import { NormalizedTable } from '../../dataSources/types';
import { Table } from '../hasura-metadata-types';
import TableHeader from '../../components/Services/Data/TableCommon/TableHeader';
import { FeatureFlagFloatingButton } from '../FeatureFlags/components/FeatureFlagFloatingButton';
import { DatabaseRelationships } from '.';

export const DatabaseRelationshipsTab = ({
  table,
  currentSource,
  migrationMode,
  driver,
  metadataTable,
}: {
  table: NormalizedTable;
  currentSource: string;
  migrationMode: boolean;
  driver: Driver;
  metadataTable: Table;
}) => {
  return (
    <RightContainer>
      <TableHeader
        dispatch={() => {}}
        table={table}
        source={currentSource}
        tabName="relationships"
        migrationMode={migrationMode}
        readOnlyMode={false}
        count={null}
        isCountEstimated
      />

      <DatabaseRelationships
        dataSourceName={currentSource}
        table={metadataTable}
      />

      <FeatureFlagFloatingButton />
    </RightContainer>
  );
};
