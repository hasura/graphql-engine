import React from 'react';
import { MdRefresh } from 'react-icons/md';
import { Button } from '../../../../../new-components/Button';
import { IndicatorCard } from '../../../../../new-components/IndicatorCard';
import { HasuraLogoFull } from '../../../../../new-components/HasuraLogo';
import { Analytics } from '../../../../Analytics';
import { NeonIcon } from './NeonIcon';
import _push from '../../../../../components/Services/Data/push';
import { emitOnboardingEvent } from '../../../utils';
import { Dispatch } from '../../../../../types';
import { skippedNeonOnboardingToConnectOtherDB } from '../../../constants';

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
      errorDescription: string | React.ReactNode;
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
  dispatch: Dispatch;
  dismiss: VoidFunction;
};

export function NeonBanner(props: Props) {
  const {
    status,
    onClickConnect,
    buttonText,
    icon,
    setStepperIndex,
    dispatch,
    dismiss,
  } = props;
  const isButtonDisabled = status.status === 'loading';

  /* This handles if a user wants to connect an existing DB or a non-postgres DB,
   it registers that user skipped onboarding and take them directly to connect database page*/
  const onClickConnectOtherDB = () => {
    dispatch(_push('/data/manage/connect'));
    emitOnboardingEvent(skippedNeonOnboardingToConnectOtherDB);
    dismiss();
  };

  return (
    <div className="border border-gray-300 border-l-4 border-l-[#297393] shadow-md rounded bg-white p-md">
      <div className="flex items-center">
        <div className="flex w-3/4 items-center">
          <div className="mr-sm">
            <div className="flex items-center">
              <HasuraLogoFull mode="brand" size="sm" />
              <div className="font-bold mx-xs">+</div>
              <a
                href="https://neon.tech/"
                target="_blank"
                rel="noopener noreferrer"
              >
                <NeonIcon />
              </a>
            </div>
          </div>
          <div className="flex flex-col w-3/4 ml-xs">
            <div className="text-md text-gray-700 mb-xs">
              Need a new database? We&apos;ve partnered with Neon to help you
              get started with a free Postgres database.
            </div>
            <div className="text-md text-gray-700">
              <a
                id="onboarding-connect-other-database-link"
                className={`w-auto text-secondary hover:text-secondary-dark  ${
                  !isButtonDisabled ? 'cursor-pointer' : 'cursor-not-allowed'
                }`}
                title={
                  isButtonDisabled ? 'Operation in progress...' : undefined
                }
                onClick={() => {
                  if (!isButtonDisabled) {
                    onClickConnectOtherDB();
                  }
                }}
              >
                Click here
              </a>{' '}
              to connect any other database.
            </div>
          </div>
        </div>
        <div className="flex w-1/4 justify-end">
          <Analytics
            name="onboarding-wizard-neon-connect-db-button"
            passHtmlAttributesToChildren
          >
            <Button
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
              <div className="text-black font-semibold text-md">
                {buttonText}
              </div>
            </Button>
          </Analytics>
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
