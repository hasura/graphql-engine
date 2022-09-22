import React, { ReactElement } from 'react';
import { Button } from '@/new-components/Button';
import { IndicatorCard } from '@/new-components/IndicatorCard';
import { NeonIcon } from './NeonIcon';

export type Props = {
  onButtonClick: VoidFunction;
  status:
    | {
        status: 'loading';
        buttonText: string;
      }
    | {
        status: 'error';
        buttonText: string;
        buttonIcon: ReactElement;
        errorTitle: string;
        errorDescription: string;
      }
    | {
        status: 'default';
        buttonText: string;
      };
};

export function NeonBanner(props: Props) {
  const { status, onButtonClick } = props;
  const isButtonDisabled = status.status === 'loading';

  return (
    <div className="border border-gray-300 border-l-4 border-l-[#297393] shadow-md rounded bg-white p-md">
      <div className="flex items-center">
        <div className="flex w-3/4 items-center">
          <div className="mr-sm">
            <NeonIcon />
          </div>
          <div className="text-lg text-gray-700 ml-sm">
            <b>Need a new database?</b> Hasura has partnered with Neon to help
            you seamlessly create your database with their serverless Postgres
            platform.
          </div>
        </div>
        <div className="flex w-1/4 justify-end">
          <Button
            data-trackid="onboarding-wizard-neon-connect-db-button"
            data-testid="onboarding-wizard-neon-connect-db-button"
            mode={status.status === 'loading' ? 'default' : 'primary'}
            isLoading={status.status === 'loading'}
            loadingText={status.buttonText}
            size="md"
            icon={status.status === 'error' ? status.buttonIcon : undefined}
            onClick={() => {
              if (!isButtonDisabled) {
                onButtonClick();
              }
            }}
            disabled={isButtonDisabled}
          >
            {status.buttonText}
          </Button>
        </div>
      </div>
      {status.status === 'error' && (
        <div className="mt-md">
          <IndicatorCard
            status="negative"
            headline={status.errorTitle}
            showIcon
          >
            {status.errorDescription}
          </IndicatorCard>
        </div>
      )}
    </div>
  );
}
