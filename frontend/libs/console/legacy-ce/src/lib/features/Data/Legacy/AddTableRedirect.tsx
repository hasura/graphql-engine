import { dataRoutes } from '@hasura/shared/utils';
import { Navigate, useParams } from 'react-router';

export const AddTableRedirect = () => {
  const params = useParams();

  if (!params.source) {
    return null;
  }

  return (
    <Navigate to={dataRoutes.addTable(params.source, params.schema)} replace />
  );
};
