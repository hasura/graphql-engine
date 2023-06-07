import { RightContainer } from '../../components/Common/Layout/RightContainer';
import { Driver } from '../../dataSources';
import { NormalizedTable } from '../../dataSources/types';
import { Table } from '../hasura-metadata-types';
import TableHeader from '../../components/Services/Data/TableCommon/TableHeader';
import { FeatureFlagFloatingButton } from '../FeatureFlags/components/FeatureFlagFloatingButton';
import { DatabaseRelationships } from '.';

type DatabaseRelationshipsTabProps = {
  table: NormalizedTable;
  currentSource: string;
  migrationMode: boolean;
  driver: Driver;
  metadataTable: Table;
};

export const DatabaseRelationshipsTab = ({
  table,
  currentSource,
  migrationMode,
  driver,
  metadataTable,
}: DatabaseRelationshipsTabProps) => {
  /**
   * This component is currently called from a few .js file sources. This means required props are not validated by the TS compiler.
   *
   * Adding this console.error calls if the two props passed to the relationships component are not found as this will break things completely.
   */
  if (!metadataTable) {
    console.error(
      '<DatabaseRelationshipsTab />: missing required prop: "metadataTable"'
    );
  }
  if (!currentSource) {
    console.error(
      '<DatabaseRelationshipsTab />: missing required prop: "currentSource"'
    );
  }
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
