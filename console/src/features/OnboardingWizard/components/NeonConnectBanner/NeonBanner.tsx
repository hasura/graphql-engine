import React from 'react';
import { MdRefresh } from 'react-icons/md';
import { Button } from '@/new-components/Button';
import { IndicatorCard } from '@/new-components/IndicatorCard';
import { NeonIcon } from './NeonIcon';

const iconMap = {
  refresh: <MdRefresh />,
};

type Status =
  | {
      status: 'loading';
    }
  | {
      status: 'error';
      errorTitle: string;
      errorDescription: string;
    }
  | {
      status: 'default';
    };

export type Props = {
  status: Status;
  onClickConnect: VoidFunction;
  buttonText: string;
  icon?: keyof typeof iconMap;
};

export function NeonBanner(props: Props) {
  const { status, onClickConnect, buttonText, icon } = props;
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
            loadingText={buttonText}
            size="md"
            icon={icon ? iconMap[icon] : undefined}
            onClick={() => {
              if (!isButtonDisabled) {
                onClickConnect();
              }
            }}
            disabled={isButtonDisabled}
          >
            {buttonText}
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
