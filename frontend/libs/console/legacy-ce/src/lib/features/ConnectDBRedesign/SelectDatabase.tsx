import { NeonBanner } from '../CloudOnboarding/OnboardingWizard/components/NeonConnectBanner/NeonBanner';
import { DatabaseKind } from './types';
import { Button } from '../../new-components/Button';
import React from 'react';
import DbConnectSVG from '../../graphics/database-connect.svg';
import { FancyRadioCards } from './components/FancyRadioCards';
import { databases } from './databases';

const enterpriseDbs: DatabaseKind[] = ['snowflake', 'athena'];

export const SelectDatabase: React.VFC = () => {
  const [selectedDb, setSelectedDb] = React.useState<DatabaseKind>('snowflake');
  return (
    <div className="flex flex-col">
      <img
        src={DbConnectSVG}
        className={`mb-md w-full`}
        //
        alt="Database Connection Diagram"
      />
      <FancyRadioCards
        items={databases}
        value={selectedDb}
        onChange={val => {
          console.log('selected value', val);
          setSelectedDb(val);
        }}
      />
      {selectedDb === 'postgres' && (
        <div className="mt-3">
          <NeonBanner
            onClickConnect={() =>
              window.alert('todo: implement Neon integration')
            }
            status={{ status: 'default' }}
            buttonText="Create a Neon Database"
            setStepperIndex={() => {}}
          />
        </div>
      )}
      {enterpriseDbs.includes(selectedDb) && (
        <div className="border border-gray-300 mt-3 shadow-md rounded bg-white p-6">
          <div className="flex items-center">
            <div className="flex flex-col w-3/4">
              <div className="text-[21px]">
                Looking to connect to{' '}
                {selectedDb === 'snowflake' ? 'a Snowflake' : 'an Athena'}{' '}
                database?
              </div>
              <div className="text-md text-gray-700">
                Deploy data connectors to add data sources such as Snowflake,
                Amazon Athena, and more to your GraphQL API.
              </div>
            </div>
            <div className="flex w-1/4 justify-end">
              <Button
                //data-testid="onboarding-wizard-neon-connect-db-button"
                mode={'primary'}
                //isLoading={status.status === 'loading'}
                //loadingText={buttonText}
                size="md"
                //icon={icon ? iconMap[icon] : undefined}
                onClick={() => {}}
                //disabled={isButtonDisabled}
              >
                <div className="text-black font-semibold text-md">
                  Enable Enterprise
                </div>
              </Button>
            </div>
          </div>
        </div>
      )}
      <Button className="mt-6 self-end">Connect Existing Database</Button>
    </div>
  );
};
