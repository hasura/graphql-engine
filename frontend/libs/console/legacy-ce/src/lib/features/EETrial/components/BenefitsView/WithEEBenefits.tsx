import * as React from 'react';
import { Dialog } from '../../../../new-components/Dialog';
import { BenefitsView } from './BenefitsView';
import { useEELicenseInfo } from '../../hooks/useEELicenseInfo';
import { EELicenseInfo } from '../../types';
import globals from '../../../../Globals';

export const WithEEBenefits: React.FC<{
  children: React.ReactNode;
  id: string;
  'data-testid'?: string;
}> = props => {
  const { children, id } = props;
  const [show, setShow] = React.useState(false);

  const {
    data: licenseData,
    error,
    isLoading,
  } = useEELicenseInfo({
    enabled: globals.consoleType === 'pro-lite',
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
    setShow(s => !s);
  };

  return (
    <>
      {show && (
        <Dialog size="md" onClose={toggleEEBenefits} hasBackdrop>
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
