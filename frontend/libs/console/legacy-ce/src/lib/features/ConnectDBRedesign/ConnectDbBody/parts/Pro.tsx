import { IndicatorCard } from '@hasura/shared/ui';
import { DriverInfo } from '@hasura/metadata/data-source';
import { SetupConnector } from '../../components';
import { ConnectButton } from '../../components/ConnectButton';
import { useNavigate } from 'react-router';
import { dataRoutes } from '@hasura/shared/utils';

export const Pro = ({
  selectedDriver,
  isDriverAvailable,
}: {
  selectedDriver: DriverInfo;
  isDriverAvailable: boolean;
}) => {
  const navigate = useNavigate();

  // GDC data sources aren't supported by self-hosted Pro
  if (selectedDriver.native === false) {
    return (
      <div className="mt-3" data-testid="gdc-requires-ee-license">
        <IndicatorCard
          status="info"
          headline={`${selectedDriver.displayName} requires an Enterprise Edition license`}
          showIcon
        >
          Connecting to {selectedDriver.displayName} is not supported with your
          current license.{' '}
          <a
            href="https://hasura.io/contact-us"
            target="_blank"
            rel="noopener noreferrer"
            className="underline"
          >
            Contact us
          </a>{' '}
          to switch to a Hasura Enterprise Edition license key.
        </IndicatorCard>
      </div>
    );
  }

  return isDriverAvailable ? (
    <ConnectButton
      selectedDriver={selectedDriver}
      isDriverAvailable={isDriverAvailable}
    />
  ) : (
    <div className="mt-3" data-testid="setup-connector">
      <SetupConnector
        selectedDriver={selectedDriver}
        onSetupSuccess={() => {
          navigate(dataRoutes.connectDatabase(selectedDriver?.name));
        }}
      />
    </div>
  );
};
