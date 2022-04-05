import React from 'react';
import { RightContainer } from '@/components/Common/Layout/RightContainer';
import { NormalizedTable } from '@/dataSources/types';
import TableHeader from '../../components/Services/Data/TableCommon/TableHeader';
import { FeatureFlagFloatingButton } from '../FeatureFlags/components/FeatureFlagFloatingButton';

export const DatabaseRelationshipsTab = ({
  tableSchema,
  currentSource,
}: {
  tableSchema: NormalizedTable;
  currentSource: string;
}) => {
  return (
    <RightContainer>
      <TableHeader
        dispatch={() => {}}
        table={tableSchema}
        source={currentSource}
        tabName="relationships"
        migrationMode={false}
        readOnlyMode={false}
        count={null}
        isCountEstimated
      />
      <div className="p-4">
        {/*  remove this title when doing the actual implementation */}
        <h2 className="text-md font-semibold mb-3.5">Table Relationships</h2>
      </div>
      <FeatureFlagFloatingButton />
    </RightContainer>
  );
};
