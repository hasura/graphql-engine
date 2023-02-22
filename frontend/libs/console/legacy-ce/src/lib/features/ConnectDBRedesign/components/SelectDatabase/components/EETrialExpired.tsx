import { Button } from '../../../../../new-components/Button';
import React from 'react';
import { FiAlertTriangle } from 'react-icons/fi';
import { DatabaseKind } from '../../../types';
import { InformationCard } from './InformationCard';

export const EETrialExpired: React.VFC<{
  onContactSales: () => void;
  selectedDb: DatabaseKind;
}> = ({ onContactSales, selectedDb }) => {
  return (
    <InformationCard>
      <div className="flex items-center">
        <div className="flex flex-col w-3/4">
          <div className="text-[21px] flex items-center gap-2">
            <FiAlertTriangle color="rgb(220 38 38)" /> Enterprise Trial Expired
          </div>
          <div className="text-md text-gray-700">
            With an Enterprise Edition license you can add data sources such as
            Snowflake, Amazon Athena, and more to your GraphQL API.
          </div>
        </div>
        <div className="flex w-1/4 justify-end">
          <Button size="md" onClick={onContactSales}>
            Contact Sales
          </Button>
        </div>
      </div>
    </InformationCard>
  );
};
