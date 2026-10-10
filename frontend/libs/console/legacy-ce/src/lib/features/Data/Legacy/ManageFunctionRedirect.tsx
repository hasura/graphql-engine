import { IndicatorCard } from '@hasura/shared/ui';
import { useDataSourceContext } from '../context/DataSourceContext';
import { useFunctionURLParameters } from '../ManageFunction/hooks/useUrlParameters';
import { Navigate } from 'react-router';
import { dataRoutes } from '@hasura/shared/utils';

export const ManageFunctionRedirect: React.FC = () => {
  const { currentSource } = useDataSourceContext();
  const urlData = useFunctionURLParameters();

  if (urlData.errorType || !urlData.qualifiedFunction) {
    return (
      <div className="p-6">
        <IndicatorCard status="negative" showIcon>
          Function not found
        </IndicatorCard>
      </div>
    );
  }

  return (
    <Navigate
      to={dataRoutes.manageFunction(
        currentSource.name,
        urlData.qualifiedFunction,
        urlData.operation,
      )}
    />
  );
};
