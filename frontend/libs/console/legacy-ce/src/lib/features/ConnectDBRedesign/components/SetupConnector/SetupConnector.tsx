import React from 'react';
import { GrConnect } from 'react-icons/gr';
import { Flex, Heading } from '@radix-ui/themes';
import { Button, IndicatorCard, Text } from '@hasura/shared/ui';
import { DriverInfo } from '@hasura/metadata/data-source';
import { DockerConfigDialog } from './parts/DockerConfigDialog';

export const SetupConnector: React.FC<{
  selectedDriver: DriverInfo;
  onSetupSuccess: () => void;
}> = ({ selectedDriver, onSetupSuccess }) => {
  const [showSetup, setShowSetup] = React.useState(false);
  return (
    <>
      <IndicatorCard
        customIcon={GrConnect}
        className="mt-3"
        status="info"
        showIcon
      >
        <Flex direction="column" data-testid="setup-data-connector-card">
          <Flex align="center" justify="between" className="w-full" gap="4">
            <Flex direction="column">
              <Heading size="3">Data Connector Required</Heading>
              <div className="mt-3">
                <Text>
                  {`The Hasura Data Connector Service is required for ${selectedDriver.displayName} databases.`}
                </Text>
              </div>
            </Flex>
            <Button mode="primary" onClick={() => setShowSetup(true)}>
              Setup Data Connector
            </Button>
          </Flex>
        </Flex>
      </IndicatorCard>
      {showSetup && (
        <DockerConfigDialog
          selectedDriver={selectedDriver}
          onCancel={() => setShowSetup(false)}
          onSetupSuccess={onSetupSuccess}
        />
      )}
    </>
  );
};
