import { useTableDefinition } from '../hooks';
import { IndicatorCard } from '@hasura/shared/ui';
import { dataRoutes, ManageDatabaseTab } from '@hasura/shared/utils';
import { Navigate } from 'react-router';

type Props = {
  subroute?: string;
  tab?: ManageDatabaseTab;
};

export const ManageDatabaseRedirect = ({ tab, subroute }: Props) => {
  const urlData = useTableDefinition();

  if (urlData.querystringParseResult === 'error')
    return (
      <IndicatorCard status="negative" showIcon>
        Something went wrong while parsing the URL parameters
      </IndicatorCard>
    );

  const { database } = urlData.data;
  if (!database) {
    return (
      <IndicatorCard status="negative">
        Source could not be found in Metadata!
      </IndicatorCard>
    );
  }

  return (
    <Navigate
      to={
        subroute
          ? dataRoutes.manageDatabaseSource(
              database,
              tab || urlData.data.schema ? 'schemas' : 'tables',
            )
          : `${dataRoutes.manageDatabaseSource(database)}${subroute}`
      }
      replace
    />
  );
};
