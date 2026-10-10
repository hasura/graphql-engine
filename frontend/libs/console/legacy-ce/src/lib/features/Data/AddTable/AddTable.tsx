import { useNavigate, useSearchParams } from 'react-router';
import { IndicatorCard, RelativeLink } from '@hasura/shared/ui';
import { dataRoutes } from '@hasura/shared/utils';
import { getDatabaseMethods } from '@hasura/metadata/data-source';
import { AddTableForm } from './AddTableForm';
import { useDataSourceContext } from '../context/DataSourceContext';
import { Heading, Strong } from '@radix-ui/themes';
import { TableBreadcrumbs } from '../ManageTable/parts';

/**
 * Route component for `data/:source/schema/:schema/table/add`.
 *
 * Resolves the source/schema from the route, gates the feature to drivers that
 * actually implement table creation (currently the Postgres family — mssql /
 * bigquery / data connectors do not implement `modify.createTable`), and
 * renders the Add Table form.
 */
export const AddTable = () => {
  const navigate = useNavigate();
  const [searchParams] = useSearchParams();
  const { currentSource } = useDataSourceContext();
  const schema = searchParams.get('schema');

  const databaseMethods = getDatabaseMethods(currentSource.kind);

  if (!databaseMethods.modify?.createTable) {
    return (
      <div className="p-6">
        <IndicatorCard status="info" headline="Adding tables is not supported">
          Creating tables from the console is not supported for the{' '}
          <Strong>{currentSource.kind}</Strong> driver. You can track existing
          tables instead.
          <RelativeLink
            to={dataRoutes.manageDatabaseSource(currentSource.name)}
          >
            Manage tables
          </RelativeLink>
        </IndicatorCard>
      </div>
    );
  }

  return (
    <div className="p-6">
      <TableBreadcrumbs dataSourceName={currentSource.name} table={undefined} />
      <div className="mb-4">
        <Heading size="4">Create New Table</Heading>
      </div>
      <AddTableForm
        source={currentSource}
        targetSchema={schema}
        onCreated={(table) => {
          navigate(dataRoutes.manageTable(currentSource.name, table, 'modify'));
        }}
      />
    </div>
  );
};

export default AddTable;
