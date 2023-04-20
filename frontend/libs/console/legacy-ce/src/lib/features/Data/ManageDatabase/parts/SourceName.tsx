import { useGetDatabaseSchemas } from '../../hooks/introspection/useDatabaseSchemas';
import { ManageDatabaseProps } from '../ManageDatabase';
import { SchemaDropdown } from './SchemaDropdown';

export function SourceName({ dataSourceName }: ManageDatabaseProps) {
  const { data } = useGetDatabaseSchemas(dataSourceName);
  const schemas = data ?? [];
  return (
    <div className="flex items-center">
      <div className="group relative flex flex-row items-center my-2">
        {schemas.length > 0 && (
          <SchemaDropdown schemas={schemas} dataSourceName={dataSourceName} />
        )}
        <h1 className="text-xl font-semibold">{dataSourceName}</h1>
      </div>
    </div>
  );
}
