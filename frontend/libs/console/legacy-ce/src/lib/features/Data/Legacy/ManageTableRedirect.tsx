import { useTableDefinition } from '../hooks';
import { IndicatorCard } from '@hasura/shared/ui';
import { dataRoutes } from '@hasura/shared/utils';
import { Navigate } from 'react-router';

export const ManageTableRedirect = () => {
  const urlData = useTableDefinition();

  if (urlData.querystringParseResult === 'error')
    return (
      <IndicatorCard status="negative" showIcon>
        Something went wrong while parsing the URL parameters
      </IndicatorCard>
    );

  const { database, table, operation } = urlData.data;
  if (!database || !table) {
    return (
      <IndicatorCard status="negative">
        Table could not be found in Metadata!
      </IndicatorCard>
    );
  }

  return (
    <Navigate to={dataRoutes.manageTable(database, table, operation)} replace />
  );
};
