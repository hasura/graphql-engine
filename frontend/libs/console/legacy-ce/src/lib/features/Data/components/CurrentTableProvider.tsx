import { Outlet, useParams } from 'react-router';
import { CurrentTableContext } from '../context/CurrentTableContext';
import { useDataSourceContext } from '../context/DataSourceContext';
import { MetadataSelectors } from '@hasura/metadata/helpers';

const CurrentTableProvider = () => {
  const params = useParams();
  const { currentSource } = useDataSourceContext();
  const metadataTable = MetadataSelectors.findMetadataTableCoarse(
    currentSource.tables,
    {
      schema: params.schema,
      name: params.name,
    },
  );

  if (!metadataTable) {
    return <div>Table not found</div>;
  }

  return (
    <CurrentTableContext.Provider
      value={{
        metadataTable,
      }}
    >
      <Outlet />
    </CurrentTableContext.Provider>
  );
};

export default CurrentTableProvider;
