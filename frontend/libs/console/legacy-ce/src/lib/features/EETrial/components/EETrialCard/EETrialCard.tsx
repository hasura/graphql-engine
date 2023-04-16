import clsx from 'clsx';
import React from 'react';
import Skeleton from 'react-loading-skeleton';
import { useQueryClient } from 'react-query';
import { Button, ButtonProps } from '../../../../new-components/Button';
import {
  EE_LICENSE_INFO_QUERY_NAME,
  EE_TRIAL_CONTACT_US_URL,
} from '../../constants';
import { EELiteAccessStatus } from '../../types';
import { EnableEEButtonWrapper } from '../EnableEnterpriseButton/EnableEEButton';
import { ErrorMessage } from '../ErrorMessage/ErrorMessage';
import { LoadingMessage } from '../LoadingMessage/LoadingMessage';
import { Analytics } from '../../../Analytics';

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
  const cardClassName = clsx(
    'flex bg-white border-2 shadow-sm p-5 rounded',
    !horizontal && 'flex-col',
    className
  );
  const isButtonFull = !horizontal;

  const handleFormClose = React.useCallback(() => {
    queryClient.invalidateQueries(EE_LICENSE_INFO_QUERY_NAME);
  }, [queryClient]);

  const enableButtonDisabled =
    eeAccess === 'expired' ||
    eeAccess === 'deactivated' ||
    eeAccess === 'forbidden';

  return (
    <div>
      <div className={cardClassName}>
        <div className="flex flex-col gap-1 flex-grow">
          {eeAccess === 'loading' ? (
            <Skeleton height={40} />
          ) : (
            <div className="text-xl text-slate-900">{cardTitle}</div>
          )}
          {eeAccess === 'loading' ? (
            <Skeleton count={2} />
          ) : (
            <div className="text-base text-muted">{cardText}</div>
          )}
        </div>
        <div
          className={clsx(
            horizontal ? 'justify-self-end ml-lg self-start' : 'mt-xs'
          )}
        >
          <EnableEEButtonWrapper
            disabled={enableButtonDisabled}
            showBenefitsView
            onFormClose={handleFormClose}
          >
            {eeAccess === 'loading' ? (
              <Skeleton width={120.35} height={35} />
            ) : (
              <Analytics name={`ee-trial-card-${id}-register-button`}>
                <Button
                  mode={buttonType}
                  className={clsx(isButtonFull && 'w-full')}
                  disabled={enableButtonDisabled}
                >
                  {buttonLabel}
                </Button>
              </Analytics>
            )}
          </EnableEEButtonWrapper>
        </div>
      </div>
      {eeAccess === 'loading' ? (
        <LoadingMessage message="Loading your EE trial information..." />
      ) : null}
      {eeAccess === 'deactivated' && (
        <ErrorMessage
          message={
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
          message={
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
