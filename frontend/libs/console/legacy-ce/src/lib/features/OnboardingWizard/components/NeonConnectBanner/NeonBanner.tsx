import React from 'react';
import { MdRefresh } from 'react-icons/md';
import { Button } from '@/new-components/Button';
import { IndicatorCard } from '@/new-components/IndicatorCard';
import { HasuraLogoFull } from '@/new-components/HasuraLogo';
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
  setStepperIndex: (index: number) => void;
};

export function NeonBanner(props: Props) {
  const { status, onClickConnect, buttonText, icon, setStepperIndex } = props;
  const isButtonDisabled = status.status === 'loading';

  return (
    <div className="border border-gray-300 border-l-4 border-l-[#297393] shadow-md rounded bg-white p-md">
      <div className="flex items-center">
        <div className="flex w-3/4 items-center">
          <div className="mr-sm">
            <div className="flex items-center">
              <HasuraLogoFull mode="brand" size="sm" />
              <div className="font-bold mx-xs">+</div>
              <NeonIcon />
            </div>
          </div>
          <div className="text-md text-gray-700 ml-xs">
            Need a new database? We&apos;ve partnered with Neon to help you get
            started.
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
                setStepperIndex(2);
                onClickConnect();
              }
            }}
            disabled={isButtonDisabled}
          >
            <div className="text-black font-semibold text-md">{buttonText}</div>
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
