import { NeonBanner } from '../../../../components/Services/Data/DataSources/CreateDataSource/Neon/components/Neon/NeonBanner';
import Globals from '../../../../Globals';
import { Button } from '../../../../new-components/Button';
import React from 'react';
import {
  EETrialActive,
  EETrialExpired,
  EETrialInactive,
  FancyRadioCards,
} from './components';
import { databases } from './databases';
import DbConnectSVG from './graphics/database-connect.svg';
import { DatabaseKind, EEState } from '../../types';

const enterpriseDbs: DatabaseKind[] = ['snowflake', 'athena'];

export interface SelectDatabaseProps {
  eeState: EEState;
  initialDb?: DatabaseKind;
  onEnableEnterpriseTrial: () => void;
  onContactSales: () => void;
}

export const SelectDatabase = ({
  eeState,
  initialDb = 'postgres',
  onEnableEnterpriseTrial,
  onContactSales,
}: SelectDatabaseProps) => {
  const [selectedDb, setSelectedDb] = React.useState<DatabaseKind>(
    initialDb || 'postgres'
  );
  const displayNeonBanner =
    Globals.consoleType === 'cloud' && !!Globals.hasuraCloudTenantId;

  const showConnectDbButton =
    (enterpriseDbs.includes(selectedDb) && eeState === 'active') ||
    !enterpriseDbs.includes(selectedDb);

  return (
    <div className="flex flex-col">
      <img
        src={DbConnectSVG}
        className={`mb-md w-full`}
        alt="Database Connection Diagram"
      />

      <FancyRadioCards
        items={databases}
        value={selectedDb}
        onChange={val => {
          setSelectedDb(val);
        }}
      />
      {selectedDb === 'postgres' && displayNeonBanner && (
        <div className="mt-3">
          <NeonBanner
            onClickConnect={() =>
              window.alert('todo: implement Neon integration')
            }
            status={{ status: 'default' }}
            buttonText="Create a Neon Database"
          />
        </div>
      )}
      {enterpriseDbs.includes(selectedDb) && eeState === 'inactive' && (
        <EETrialInactive
          selectedDb={selectedDb}
          onEnableEnterpriseTrial={onEnableEnterpriseTrial}
        />
      )}
      {enterpriseDbs.includes(selectedDb) && eeState === 'active' && (
        <EETrialActive selectedDb={selectedDb} />
      )}
      {enterpriseDbs.includes(selectedDb) && eeState === 'expired' && (
        <EETrialExpired
          selectedDb={selectedDb}
          onContactSales={onContactSales}
        />
      )}
      {showConnectDbButton && (
        <Button className="mt-6 self-end">Connect Existing Database</Button>
      )}
    </div>
  );
};
