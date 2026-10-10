import React from 'react';
import { MdRefresh } from 'react-icons/md';
import {
  Button,
  Card,
  HasuraLogoFull,
  IndicatorCard,
  Text,
  useAppearance,
} from '@hasura/shared/ui';
import { Analytics } from '@hasura/shared/analytics';
import { NeonIcon } from './NeonIcon';
import { emitOnboardingEvent } from '../../../utils';
import { skippedNeonOnboardingToConnectOtherDB } from '../../../constants';
import { useNavigate } from 'react-router';
import { Flex, Link } from '@radix-ui/themes';

const iconMap = {
  refresh: MdRefresh,
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

export type NeonBannerProps = {
  status: Status;
  onClickConnect: VoidFunction;
  buttonText: string;
  icon?: keyof typeof iconMap;
  setStepperIndex?: (index: number) => void;
  dismiss?: VoidFunction;
};

export function NeonBanner(props: NeonBannerProps) {
  const { status, onClickConnect, buttonText, icon, setStepperIndex, dismiss } =
    props;
  const navigate = useNavigate();
  const { appearance } = useAppearance();
  const isButtonDisabled = status.status === 'loading';

  /* This handles if a user wants to connect an existing DB or a non-postgres DB,
   it registers that user skipped onboarding and take them directly to connect database page*/
  const onClickConnectOtherDB = () => {
    navigate('/data/manage/connect');
    emitOnboardingEvent(skippedNeonOnboardingToConnectOtherDB);
    dismiss?.();
  };

  return (
    <Card mode="default" className="mt-4">
      <Flex align="center">
        <Flex align="center" className="w-3/4">
          <div className="mr-2">
            <Flex align="center" gap="2">
              <HasuraLogoFull
                mode={appearance === 'dark' ? 'primary' : 'brand'}
                size="sm"
              />
              <Text weight="bold">+</Text>
              <a
                href="https://neon.tech/"
                target="_blank"
                rel="noopener noreferrer"
              >
                <NeonIcon color={appearance === 'dark' ? 'white' : 'black'} />
              </a>
            </Flex>
          </div>
          <Flex direction="column" gap="2" className="w-3/4 ml-xs">
            <Text>
              Need a new database? We&apos;ve partnered with Neon to help you
              get started with a free Postgres database.
            </Text>
            <Text>
              <Link
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
              </Link>{' '}
              to connect any other database.
            </Text>
          </Flex>
        </Flex>
        <Flex justify="end" className="w-1/4">
          <Analytics
            name="onboarding-wizard-neon-connect-db-button"
            passHtmlAttributesToChildren
          >
            <Button
              data-testid="onboarding-wizard-neon-connect-db-button"
              mode={status.status === 'loading' ? 'default' : 'primary'}
              loading={status.status === 'loading'}
              loadingText={buttonText}
              size="md"
              leftIcon={icon ? iconMap[icon] : undefined}
              onClick={() => {
                if (!isButtonDisabled) {
                  setStepperIndex?.(2);
                  onClickConnect();
                }
              }}
              disabled={isButtonDisabled}
            >
              {buttonText}
            </Button>
          </Analytics>
        </Flex>
      </Flex>
      {status.status === 'error' && (
        <div className="mt-4">
          <IndicatorCard
            status="negative"
            headline={status.errorTitle}
            showIcon
          >
            {status.errorDescription}
          </IndicatorCard>
        </div>
      )}
    </Card>
  );
}
