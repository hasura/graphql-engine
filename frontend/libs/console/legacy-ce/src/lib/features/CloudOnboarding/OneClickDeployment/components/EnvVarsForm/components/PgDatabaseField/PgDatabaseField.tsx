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

  const toggleShowNeonButton = () => {
    setShowNeonButton((s) => !s);
  };

  const neonIntegrationStatus = useNeonIntegrationForOneClickDeployment();

  const neonButtonProps = React.useMemo(
    () =>
      transformNeonIntegrationStatusToNeonButtonProps(neonIntegrationStatus),
    [neonIntegrationStatus],
  );

  // The populated Neon database URL is fully derived from the integration
  // status, so we compute it during render instead of mirroring it into state
  // via an effect (which would cause an extra render pass).
  const neonDBURL =
    neonButtonProps.status.status === 'success'
      ? (neonButtonProps.dbURL ?? '')
      : '';

  // Writing the resolved URL into the form is a genuine side effect to an
  // external store (react-hook-form), so it stays in an effect — now with a
  // complete dependency list.
  React.useEffect(() => {
    const dbUrl = neonButtonProps.dbURL;
    if (neonButtonProps.status.status === 'success' && dbUrl) {
      setValue(dbEnvVar.Name, dbUrl);
    }
  }, [
    neonButtonProps.status.status,
    neonButtonProps.dbURL,
    dbEnvVar.Name,
    setValue,
  ]);

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
