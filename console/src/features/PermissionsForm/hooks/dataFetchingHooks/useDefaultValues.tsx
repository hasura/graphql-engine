import { NormalizedTable } from '@/dataSources/types';
import { useMetadataTablePermissions } from '@/features/MetadataAPI';
import { useSingleTable } from '@/hooks';
import { useAppSelector } from '@/store';
import { currentDriver } from '@/dataSources';

import { getCurrentRole } from '../../utils';

import { QueryType } from '../../types';

namespace Format {
  export const getCheckType = (check?: string | null) => {
    if (!check) {
      return 'none';
    }

    if (check === '{}') {
      return 'no_checks';
    }

    return 'custom';
  };

  interface GetRowCountArgs {
    currentQueryPermissions?: Record<string, any>;
  }

  export const getRowCount = ({ currentQueryPermissions }: GetRowCountArgs) => {
    return `${currentQueryPermissions?.limit ?? 0}`;
  };

  interface GetCheckArgs {
    currentQueryPermissions?: Record<string, any>;
    type: 'check' | 'filter';
  }

  export const getCheck = ({ currentQueryPermissions, type }: GetCheckArgs) => {
    const check = currentQueryPermissions?.[type];
    return check ? JSON.stringify(check) : '';
  };

  interface FormatColumnsArgs {
    table?: NormalizedTable | null;
    currentQueryPermissions?: Record<string, any>;
  }

  export const formatColumns = ({
    table,
    currentQueryPermissions,
  }: FormatColumnsArgs) => {
    const allColumns = table?.columns?.map(({ column_name }) => column_name);
    const selectedColumns = currentQueryPermissions?.columns;

    if (!allColumns || !selectedColumns) {
      return {};
    }

    return allColumns?.reduce((acc, column) => {
      const selected = selectedColumns?.includes(column);
      acc[column] = selected;
      return acc;
    }, {} as Record<typeof selectedColumns, boolean>);
  };

  interface GetPresetArgs {
    currentQueryPermissions?: Record<string, any>;
  }

  export const getPresets = ({ currentQueryPermissions }: GetPresetArgs) => {
    const set = Object.entries(currentQueryPermissions?.set || {}) as Array<
      [string, string]
    >;

    return set.map(([columnName, value]) => {
      return {
        columnName,
        presetType: value.startsWith('x-hasura')
          ? 'from session variable'
          : 'static',
        value,
      };
    });
  };

  export const getAllRowChecks = (
    currentQuery: QueryType,
    permissions?: Record<string, any>
  ) => {
    const allChecks = Object.entries(permissions || {}) as [QueryType, any];

    return allChecks
      .filter(([queryType]) => queryType !== currentQuery)
      .map(([queryType, permission]) => {
        if (['insert', 'update'].includes(queryType)) {
          return { queryType, value: JSON.stringify(permission.check || {}) };
        }

        return {
          queryType,
          value: JSON.stringify(permission.filter || {}),
        };
      });
  };
}

export interface UseDefaultValuesArgs {
  schemaName: string;
  tableName: string;
  roleName: string;
  queryType: QueryType;
}

const useLoadPermissions = ({
  schemaName,
  tableName,
  roleName,
  queryType,
}: UseDefaultValuesArgs) => {
  const dataSource: string =
    useAppSelector(state => state.tables.currentDataSource) || 'default';

  const {
    data: table,
    isLoading: tableLoading,
    isError: tableError,
  } = useSingleTable({
    source: dataSource,
    driver: currentDriver,
    table: { name: tableName, schema: schemaName },
  });

  const {
    data: permissions,
    isLoading: permissionsLoading,
    isError: permissionsError,
  } = useMetadataTablePermissions(
    {
      schema: schemaName,
      name: tableName,
    },
    dataSource
  );

  const currentRolePermissions = getCurrentRole({ permissions, roleName });

  const currentQueryPermissions =
    currentRolePermissions?.permissions?.[queryType];

  const allRowChecks = Format.getAllRowChecks(
    queryType,
    currentRolePermissions?.permissions
  );

  const isLoading = tableLoading || permissionsLoading;
  const isError = tableError || permissionsError;

  return {
    data: {
      currentQueryPermissions,
      allRowChecks,
      table,
    },
    isLoading,
    isError,
  };
};

export const useDefaultValues = (args: UseDefaultValuesArgs) => {
  const {
    data: { currentQueryPermissions, table, allRowChecks },
    isLoading,
    isError,
  } = useLoadPermissions(args);

  const check = Format.getCheck({ currentQueryPermissions, type: 'check' });
  const filter = Format.getCheck({
    currentQueryPermissions,
    type: 'filter',
  });
  const checkType = Format.getCheckType(check);
  const filterType = Format.getCheckType(filter);
  const rowCount = Format.getRowCount({ currentQueryPermissions });
  const columns = Format.formatColumns({ table, currentQueryPermissions });
  const presets = Format.getPresets({ currentQueryPermissions });

  return {
    data: {
      checkType,
      filterType,
      check,
      filter,
      rowCount,
      columns,
      presets,
      backendOnly: currentQueryPermissions?.backend_only || false,
      aggregationEnabled: currentQueryPermissions?.allow_aggregations || false,
      clonePermissions: [],
      allRowChecks,
    },
    isLoading,
    isError,
  };
};
