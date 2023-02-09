import { Button } from '@/new-components/Button';
import React from 'react';
import { DatabaseKind } from '../../../types';
import { dbDisplayNames } from '../databases';
import { indefiniteArticle } from '../utils';
import { InformationCard } from './InformationCard';

export const EETrialInactive: React.VFC<{
  onEnableEnterpriseTrial: () => void;
  selectedDb: DatabaseKind;
}> = ({ onEnableEnterpriseTrial, selectedDb }) => {
  const dbWithArticle = `${indefiniteArticle(selectedDb)} ${
    dbDisplayNames[selectedDb]
  }`;
  return (
    <InformationCard>
      <div className="flex items-center">
        <div className="flex flex-col w-3/4">
          <div className="text-[21px]">
            {`Looking to connect to ${dbWithArticle} database?`}
          </div>
          <div className="text-md text-gray-700">
            Deploy data connectors to add data sources such as Snowflake, Amazon
            Athena, and more to your GraphQL API.
          </div>
        </div>
        <div className="flex w-1/4 justify-end">
          <Button mode={'primary'} size="md" onClick={onEnableEnterpriseTrial}>
            Enable Enterprise
          </Button>
        </div>
      </div>
    </InformationCard>
  );
};
