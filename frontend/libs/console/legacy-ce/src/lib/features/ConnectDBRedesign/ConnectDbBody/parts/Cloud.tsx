import { NeonConnect } from '../../../../components/Services/Data/DataSources/CreateDataSource/Neon';
import { IndicatorCard } from '../../../../new-components/IndicatorCard';
import { useAppDispatch } from '../../../../storeHooks';
import { DriverInfo } from '../../../DataSource';
import { ConnectButton } from '../../components/ConnectButton';

export const Cloud = ({
  selectedDriver,
  isDriverAvailable,
}: {
  selectedDriver: DriverInfo;
  isDriverAvailable: boolean;
}) => {
  const dispatch = useAppDispatch();

  return (
    <>
      {selectedDriver?.name === 'postgres' && (
        <div className="mt-3" data-testid="neon-connect">
          <NeonConnect
            dispatch={dispatch}
            connectDbUrl={'/data/v2/manage/connect'}
          />
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
