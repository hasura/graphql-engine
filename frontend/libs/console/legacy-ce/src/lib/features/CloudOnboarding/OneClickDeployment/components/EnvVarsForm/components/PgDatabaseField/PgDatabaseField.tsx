import React from 'react';
import { useFormContext } from 'react-hook-form';
import { Analytics } from '@hasura/shared/analytics';
import { RequiredEnvVar } from '../../../../types';
import { useNeonIntegrationForOneClickDeployment } from '../../hooks';
import { transformNeonIntegrationStatusToNeonButtonProps } from '../../utils';
import { InputModeToggle } from './InputModeToggle';
import { InputWrapper } from './InputWrapper';
import { Flex, Text } from '@radix-ui/themes';

type PgDatabaseFieldProps = {
  dbEnvVar: RequiredEnvVar;
};

export function PgDatabaseField(props: PgDatabaseFieldProps) {
  const { dbEnvVar } = props;
  const { setValue } = useFormContext();
  const [showNeonButton, setShowNeonButton] = React.useState(true);
  const [neonDBURL, setNeonDBURL] = React.useState('');

  const toggleShowNeonButton = () => {
    setShowNeonButton((s) => !s);
  };

  const neonIntegrationStatus = useNeonIntegrationForOneClickDeployment();

  const neonButtonProps = React.useMemo(
    () =>
      transformNeonIntegrationStatusToNeonButtonProps(neonIntegrationStatus),
    [neonIntegrationStatus],
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
      <Flex align="center" justify="between">
        <Text weight="bold">{dbEnvVar.Name} *</Text>
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
      </Flex>
      <Text size="2">{dbEnvVar.Description}</Text>
      <InputWrapper
        neonDBURL={neonDBURL}
        showNeonButton={showNeonButton}
        neonButtonProps={neonButtonProps}
        dbEnvVar={dbEnvVar}
      />
    </>
  );
}
