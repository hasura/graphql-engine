import { useEffect } from 'react';
import { NeonIntegrationStatus } from '../../../../components/Services/Data/DataSources/CreateDataSource/Neon/useNeonIntegration';
import {
  neonOAuthStartVariables,
  neonDbCreationStartVariables,
  hasuraSourceCreationStartVariables,
  installTemplateStartVariables,
  getNeonOnboardingErrorVariables,
} from '../../constants';
import { emitOnboardingEvent } from '../../utils';

export function useEmitOnboardingEvents(
  neonIntegrationStatus: NeonIntegrationStatus,
  installingTemplate: boolean
) {
  useEffect(() => {
    switch (neonIntegrationStatus.status) {
      case 'authentication-error':
        emitOnboardingEvent(getNeonOnboardingErrorVariables('authentication'));
        break;
      case 'neon-database-creation-error':
        emitOnboardingEvent(
          getNeonOnboardingErrorVariables('neon-database-creation')
        );
        break;
      case 'hasura-source-creation-error':
        emitOnboardingEvent(
          getNeonOnboardingErrorVariables('hasura-source-creation')
        );
        break;
      case 'authentication-loading':
        emitOnboardingEvent(neonOAuthStartVariables);
        break;
      case 'neon-database-creation-loading':
        emitOnboardingEvent(neonDbCreationStartVariables);
        break;
      case 'hasura-source-creation-loading':
        emitOnboardingEvent(hasuraSourceCreationStartVariables);
        break;
      default:
        break;
    }
  }, [neonIntegrationStatus.status]);

  useEffect(() => {
    if (installingTemplate) {
      emitOnboardingEvent(installTemplateStartVariables);
    }
  }, [installingTemplate]);
}
