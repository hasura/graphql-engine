import * as React from 'react';
import { Dialog } from '@hasura/shared/ui';
import { useAppContext } from '@hasura/shared/context';
import { BenefitsView } from './BenefitsView';
import { useEELicenseInfo } from '../../hooks/useEELicenseInfo';
import { EELicenseInfo } from '../../types';

export const WithEEBenefits: React.FC<{
  children: React.ReactNode;
  id: string;
  'data-testid'?: string;
}> = (props) => {
  const { children, id } = props;
  const [show, setShow] = React.useState(false);
  const { envVars } = useAppContext();

  const {
    data: licenseData,
    error,
    isLoading,
  } = useEELicenseInfo({
    enabled: envVars.consoleType === 'pro-lite',
  });

  let licenseInfo: EELicenseInfo;
  if (isLoading || error || !licenseData) {
    licenseInfo = {
      status: 'none',
      type: 'trial',
      expiry_at: new Date(),
    };
  } else {
    licenseInfo = licenseData;
  }

  const toggleEEBenefits = () => {
    setShow((s) => !s);
  };

  return (
    <>
      {show && (
        <Dialog size="md" onClose={toggleEEBenefits}>
          <BenefitsView licenseInfo={licenseInfo} />
        </Dialog>
      )}
      <div
        role="button"
        onClick={toggleEEBenefits}
        id={id}
        data-testid={props['data-testid']}
      >
        {children}
      </div>
    </>
  );
};
