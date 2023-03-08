import React from 'react';
import { useFormContext } from 'react-hook-form';
import { Analytics } from '../../../../../../Analytics';
import { RequiredEnvVar } from '../../../../types';
import { useNeonIntegrationForOneClickDeployment } from '../../hooks';
import { transformNeonIntegrationStatusToNeonButtonProps } from '../../utils';
import { InputModeToggle } from './InputModeToggle';
import { InputWrapper } from './InputWrapper';

type PgDatabaseFieldProps = {
  dbEnvVar: RequiredEnvVar;
};

export function PgDatabaseField(props: PgDatabaseFieldProps) {
  const { dbEnvVar } = props;
  const { setValue } = useFormContext();
  const [showNeonButton, setShowNeonButton] = React.useState(true);
  const [neonDBURL, setNeonDBURL] = React.useState('');

  const toggleShowNeonButton = () => {
    setShowNeonButton(s => !s);
  };

  const neonIntegrationStatus = useNeonIntegrationForOneClickDeployment();

  const neonButtonProps = React.useMemo(
    () =>
      transformNeonIntegrationStatusToNeonButtonProps(neonIntegrationStatus),
    [neonIntegrationStatus]
  );

  React.useEffect(() => {
    const dbUrl = neonButtonProps.dbURL;
    if (neonButtonProps.status.status === 'success' && dbUrl) {
      setNeonDBURL(dbUrl);
      setValue(dbEnvVar.Name, dbUrl);
    }
  }, [neonButtonProps.status.status]);

  return (
    <>
      <div className="flex items-center justify-between">
        <div className="font-bold text-gray-600 text-md">{dbEnvVar.Name} *</div>
        <div>
          {neonDBURL ? null : (
            <Analytics
              name="one-click-deployment-db-input-toggle"
              passHtmlAttributesToChildren
            >
              <InputModeToggle
                showNeonButton={showNeonButton}
                toggleShowNeonButton={toggleShowNeonButton}
                disabled={
                  neonButtonProps.status.status === 'loading' ||
                  neonDBURL.length > 0
                }
              />
            </Analytics>
          )}
        </div>
      </div>
      <div className="mb-xs font-normal text-gray-600 text-sm">
        {dbEnvVar.Description}
      </div>
      <InputWrapper
        neonDBURL={neonDBURL}
        showNeonButton={showNeonButton}
        neonButtonProps={neonButtonProps}
        dbEnvVar={dbEnvVar}
      />
    </>
  );
}
