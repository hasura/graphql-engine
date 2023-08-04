import { MetadataSelectors } from '../../hasura-metadata-api';
import { MetadataWrapper, ReactQueryStatusUI } from '../components';
import { useTableDefinition } from '../hooks';
import { ManageDatabase } from './ManageDatabase';

export const ManageDatabaseRoute = () => {
  const urlData = useTableDefinition(window.location);

  if (urlData.querystringParseResult === 'error')
    return <>Something went wrong while parsing the URL parameters</>;

  const { database } = urlData.data;

  return (
    <MetadataWrapper
      selector={MetadataSelectors.findSource(database)}
      render={({ data: source }) => {
        // if we don't find the source, report an error:
        if (!source) {
          return (
            <ReactQueryStatusUI
              status="error"
              error={{
                message: `Source ${database}" could not be found in Metadata!`,
              }}
            />
          );
        }

        return <ManageDatabase dataSourceName={database} />;
      }}
    />
  );
};
