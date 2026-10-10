import { NeonConnect } from '../../../CloudOnboarding/NeonOnboardingWizard/components/NeonConnect';
import { IndicatorCard } from '@hasura/shared/ui';
import { DriverInfo } from '@hasura/metadata/data-source';
import { ConnectButton } from '../../components/ConnectButton';
import { dataRoutes } from '@hasura/shared/utils';

export const Cloud = ({
  selectedDriver,
  isDriverAvailable,
}: {
  selectedDriver: DriverInfo;
  isDriverAvailable: boolean;
}) => {
  return (
    <>
      {selectedDriver?.name === 'postgres' && (
        <div className="mt-3" data-testid="neon-connect">
          <NeonConnect connectDbUrl={dataRoutes.connectDatabase()} />
        </div>
      )}

      {!isDriverAvailable ? (
        <div className="mt-3" data-testid="cloud-driver-not-available">
          <IndicatorCard
            status="negative"
            headline="Cannot find the corresponding driver info"
          >
            The response from<code>list_source_kinds</code>did not return your
            selected driver. Please verify if the data connector agent is
            reachable from your Hasura instance.
          </IndicatorCard>
        </div>
      ) : (
        <ConnectButton selectedDriver={selectedDriver} />
      )}
    </>
  );
};
