import { Analytics, REDACT_EVERYTHING } from '@hasura/shared/analytics';
import { useDocumentTitle } from '@hasura/shared/hooks';
import ApiRequest from './ApiRequest/ApiRequest';
import { useMetadata } from '@hasura/metadata/api';
import { useAuthContext } from '@hasura/shared/context';
import { hasDataAccess } from '../../../shared/auth/privileges';

const ApiExplorer = () => {
  useDocumentTitle('API Explorer | Hasura');
  const { privileges } = useAuthContext();
  // the metadata API is admin-only, GraphQL-only collaborators can't export it.
  const { data: numberOfTables } = useMetadata(
    (m) =>
      m.metadata.sources.reduce((acc, source) => acc + source.tables.length, 0),
    { staleTime: 5 * 60000, enabled: hasDataAccess(privileges) },
  );

  return (
    <Analytics name="ApiExplorer" {...REDACT_EVERYTHING}>
      <div id="apiRequestBlock" className="px-4 h-full w-full">
        <ApiRequest numberOfTables={numberOfTables ?? 0} />
      </div>
    </Analytics>
  );
};

export default ApiExplorer;
