import clsx from 'clsx';
import React from 'react';
import { Card, Flex, Heading } from '@radix-ui/themes';
import { useQueryClient } from '@tanstack/react-query';
import {
  Button,
  ButtonProps,
  ErrorMessage,
  Spinner,
  Text,
} from '@hasura/shared/ui';
import {
  EE_LICENSE_INFO_QUERY_NAME,
  EE_TRIAL_CONTACT_US_URL,
} from '../../constants';
import { EELiteAccessStatus } from '../../types';
import { EnableEEButtonWrapper } from '../EnableEnterpriseButton/EnableEEButton';
import { Analytics } from '@hasura/shared/analytics';
import { Skeleton } from '@radix-ui/themes';

interface EETrialCardProps extends React.ComponentProps<'div'> {
  /**
   *  The card title.
   */
  cardTitle?: React.ReactNode;
  /**
   * The card text
   */
  cardText?: React.ReactNode;
  /**
   * The card button label
   */
  buttonLabel?: string;
  /**
   * The card button type
   */
  buttonType?: ButtonProps['mode'];
  /**
   * The card orientation
   */
  horizontal?: boolean;
  /**
   * EE lite access status
   */
  eeAccess?: EELiteAccessStatus;

  id: string;
}

export const EETrialCard = ({
  cardTitle = '',
  cardText = '',
  horizontal = false,
  buttonLabel = 'Enable Enterprise',
  buttonType = 'primary',
  eeAccess = 'active',
  className,
  id,
}: EETrialCardProps) => {
  const queryClient = useQueryClient();
  const isButtonFull = !horizontal;

  const handleFormClose = React.useCallback(() => {
    queryClient.invalidateQueries({ queryKey: EE_LICENSE_INFO_QUERY_NAME });
  }, [queryClient]);

  const enableButtonDisabled =
    eeAccess === 'expired' ||
    eeAccess === 'deactivated' ||
    eeAccess === 'forbidden';

  const isLoading = eeAccess === 'loading';
  return (
    <div>
      <Card size="3">
        <Flex gap="2">
          <Flex direction="column" gap="2" className="grow">
            <Skeleton loading={isLoading}>
              <Heading size="4">{cardTitle}</Heading>
            </Skeleton>
            <Skeleton loading={isLoading}>
              <Text>{cardText}</Text>
            </Skeleton>
          </Flex>
          <div className="mt-2">
            <EnableEEButtonWrapper
              disabled={enableButtonDisabled}
              showBenefitsView
              onFormClose={handleFormClose}
            >
              <Skeleton loading={isLoading}>
                <Analytics name={`ee-trial-card-${id}-register-button`}>
                  <Button
                    mode={buttonType}
                    className={clsx(isButtonFull && 'w-full')}
                    disabled={enableButtonDisabled}
                  >
                    {buttonLabel}
                  </Button>
                </Analytics>
              </Skeleton>
            </EnableEEButtonWrapper>
          </div>
        </Flex>
      </Card>
      {eeAccess === 'loading' ? (
        <Spinner>Loading your EE trial information...</Spinner>
      ) : null}
      {eeAccess === 'deactivated' && (
        <ErrorMessage
          error={
            <span>
              Your EE trial has been deactivated. Please{' '}
              <a
                href={EE_TRIAL_CONTACT_US_URL}
                target="_blank"
                rel="noopener noreferrer"
                className="text-inherit underline"
              >
                contact us
              </a>{' '}
              for more info.
            </span>
          }
        />
      )}
      {eeAccess === 'expired' && (
        <ErrorMessage
          error={
            <span>
              Your EE trial has expired. Please{' '}
              <a
                href={EE_TRIAL_CONTACT_US_URL}
                target="_blank"
                rel="noopener noreferrer"
                className="text-inherit underline"
              >
                contact us
              </a>{' '}
              for more info.
            </span>
          }
        />
      )}
    </div>
  );
};
