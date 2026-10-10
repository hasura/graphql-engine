import { Analytics, REDACT_EVERYTHING } from '@hasura/shared/analytics';
import { useDocumentTitle } from '@hasura/shared/hooks';
import ApiRequest from './ApiRequest/ApiRequest';
import { useMetadata } from '@hasura/metadata/api';

const ApiExplorer = () => {
  useDocumentTitle('API Explorer | Hasura');
  const { data: numberOfTables } = useMetadata((m) =>
    m.metadata.sources.reduce((acc, source) => acc + source.tables.length, 0),
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
