import * as React from 'react';
import { Button } from '../../../../new-components/Button';
import { FaExternalLinkAlt } from 'react-icons/fa';
import moment from 'moment';
import { LabelValue } from './LabelValue';
import {
  useEELiteAccess,
  EELiteAccess,
  EE_TRIAL_CONTACT_US_URL,
  EETrialCard,
} from '../../../../features/EETrial';
import globals from '../../../../Globals';

export const EECTAButton: React.VFC<{
  text: string;
  className?: string;
}> = props => {
  const { className, text } = props;
  return (
    <a
      href={EE_TRIAL_CONTACT_US_URL}
      target="_blank"
      rel="noopener noreferrer"
      className={className}
    >
      <Button
        icon={<FaExternalLinkAlt className="text-sm" />}
        iconPosition="end"
        className="font-weight-700 text-md"
      >
        {text}
      </Button>
    </a>
  );
};

export const EELicenseInfo: React.VFC<{ className?: string }> = props => {
  const { className } = props;
  const eeLite = useEELiteAccess(globals);

  if (eeLite.access === 'forbidden') {
    return null;
  }

  return (
    <div className={className}>
      <EELicenseInfoUI info={eeLite} />
    </div>
  );
};

export const EELicenseInfoUI: React.VFC<{
  info: EELiteAccess;
}> = props => {
  const { info } = props;
  switch (info.access) {
    case 'eligible': {
      return (
        <div className="max-w-3xl">
          <div className="mb-xs">
            <LabelValue
              label="Enterprise Edition"
              value={
                <EETrialCard
                  cardTitle="Activate your free Hasura Enterprise trial license"
                  className="mt-xs"
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
      const expiryDate = moment(info.license.expiry_at);
      return (
        <div>
          <div className="mb-xs">
            <LabelValue
              label="Enterprise Edition Expiry Date"
              value={`${expiryDate.format(
                'D MMMM, YYYY'
              )} (${expiryDate.fromNow()})`}
            />
          </div>
          <EECTAButton
            text={info.access === 'active' ? 'Get in touch' : 'Renew License'}
            className="mt-md"
          />
        </div>
      );
    case 'deactivated':
      return (
        <div>
          <div className="mb-xs">
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
