import * as React from 'react';
import { Button } from '@hasura/shared/ui';
import { FaExternalLinkAlt } from 'react-icons/fa';
import { format, formatDistanceToNow } from 'date-fns';
import { LabelValue } from './LabelValue';
import {
  useEELiteAccess,
  EELiteAccess,
  EE_TRIAL_CONTACT_US_URL,
  EETrialCard,
} from '../../../../features/EETrial';

export const EECTAButton: React.FC<{
  text: string;
  className?: string;
}> = (props) => {
  const { className, text } = props;
  return (
    <a
      href={EE_TRIAL_CONTACT_US_URL}
      target="_blank"
      rel="noopener noreferrer"
      className={className}
    >
      <Button rightIcon={FaExternalLinkAlt} className="font-weight-700 text-md">
        {text}
      </Button>
    </a>
  );
};

export const EELicenseInfo: React.FC<{ className?: string }> = ({
  className,
}) => {
  const eeLite = useEELiteAccess();

  if (eeLite.access === 'forbidden') {
    return null;
  }

  return (
    <div className={className}>
      <EELicenseInfoUI info={eeLite} />
    </div>
  );
};

export const EELicenseInfoUI: React.FC<{
  info: EELiteAccess;
}> = (props) => {
  const { info } = props;
  switch (info.access) {
    case 'eligible': {
      return (
        <div className="max-w-3xl">
          <div className="mb-1">
            <LabelValue
              label="Enterprise Edition"
              value={
                <EETrialCard
                  cardTitle="Activate your free Hasura Enterprise trial license"
                  className="mt-2"
                  cardText="Unlock extra observability, security, and performance features for your Hasura instance."
                  eeAccess={info.access}
                  horizontal
                  id="settings-about-ee"
                />
              }
            />
          </div>
        </div>
      );
    }
    case 'active':
    case 'expired':
      const expiryDate = info.license.expiry_at ?? new Date();
      return (
        <div>
          <div className="mb-1">
            <LabelValue
              label="Enterprise Edition Expiry Date"
              value={`${format(
                expiryDate,
                'd MMMM, yyyy',
              )} (${formatDistanceToNow(expiryDate, { addSuffix: true })})`}
            />
          </div>
          <EECTAButton
            text={info.access === 'active' ? 'Get in touch' : 'Renew License'}
            className="mt-4"
          />
        </div>
      );
    case 'deactivated':
      return (
        <div>
          <div className="mb-1">
            <LabelValue label="Enterprise Edition" value={`Deactivated`} />
          </div>
          <EECTAButton text="Get in touch" />
        </div>
      );
    case 'loading':
    default:
      return null;
  }
};
