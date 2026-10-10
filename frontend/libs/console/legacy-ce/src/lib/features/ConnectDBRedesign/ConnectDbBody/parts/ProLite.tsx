import { IndicatorCard } from '@hasura/shared/ui';
import { DriverInfo } from '@hasura/metadata/data-source';
import { EELiteAccess, EETrialCard } from '../../../EETrial';
import { SetupConnector } from '../../components';
import { ConnectButton } from '../../components/ConnectButton';
import { eeCardContentMap } from '../../constants';
import { indefiniteArticle } from '../../utils';
import { dataRoutes } from '@hasura/shared/utils';
import { useNavigate } from 'react-router';

export const ProLite = ({
  selectedDriver,
  isDriverAvailable,
  eeLicenseInfo,
}: {
  selectedDriver: DriverInfo;
  isDriverAvailable: boolean;
  eeLicenseInfo: EELiteAccess['access'];
}) => {
  const navigate = useNavigate();
  const dbWithArticle = `${indefiniteArticle(selectedDriver.displayName)} ${
    selectedDriver.displayName
  }`;

  return (
    <>
      {selectedDriver?.enterprise &&
        (() => {
          switch (eeLicenseInfo) {
            case 'active':
              return !isDriverAvailable ? (
                <div className="mt-3" data-testid="setup-connector">
                  <SetupConnector
                    selectedDriver={selectedDriver}
                    onSetupSuccess={() => {
                      navigate(
                        dataRoutes.connectDatabase(selectedDriver?.name),
                      );
                    }}
                  />
                </div>
              ) : null;
            case 'forbidden':
              /**
               *
               * The only way "forbidden" happens here is if the licensing API is not reachable.
               *
               */
              return (
                <div className="mt-3" data-testid="license-forbidden-api-error">
                  <IndicatorCard
                    status="negative"
                    headline="Error Loading License"
                  >
                    Unable to determine your Enterprise License status.
                  </IndicatorCard>
                </div>
              );

            /**
             * Being verbose for state clarity
             */
            case 'loading':
            case 'deactivated':
            case 'expired':
            case 'eligible':
              return (
                <div className="mt-3" data-testid="ee-trial-card">
                  <EETrialCard
                    eeAccess={eeLicenseInfo}
                    id={dbWithArticle.replace(' ', '-')}
                    horizontal
                    {...eeCardContentMap(dbWithArticle)[eeLicenseInfo]}
                  />
                </div>
              );
          }
        })()}

      {(!selectedDriver?.enterprise ||
        (eeLicenseInfo === 'active' && isDriverAvailable)) && (
        <ConnectButton selectedDriver={selectedDriver} />
      )}
    </>
  );
};
