import { useMemo } from 'react';
import { To } from 'react-router';
import { Analytics } from '@hasura/shared/analytics';
import { dataRoutes, getTableDisplayName } from '@hasura/shared/utils';
import PermissionsEditor from './PermissionsEditor';
import { LearnMoreLink, RelativeLink, Text } from '@hasura/shared/ui';
import { useServerConfig } from '@hasura/metadata/api';
import { GetFunctionDefinitionResult } from '@hasura/metadata/data-source';
import { MetadataFunction, Source } from '@hasura/shared/types';
import { Code } from '@radix-ui/themes';

const PermissionServerFlagNote = ({ isEditable = false }) =>
  !isEditable ? (
    <Text as="div">
      Function will be exposed automatically if there are SELECT permissions for
      the role. To expose query functions to roles explicitly, set{' '}
      <Code>HASURA_GRAPHQL_INFER_FUNCTION_PERMISSIONS=false</Code> on the
      server.
      <LearnMoreLink href="https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/custom-functions.html#api-custom-functions" />
    </Text>
  ) : (
    <Text as="div">
      The function will be exposed to the role if SELECT permissions are enabled
      and function permissions are enabled for the role.
    </Text>
  );

const getRefTablePermissionsRoute = (
  source: string,
  func: MetadataFunction,
  functionDefinition: GetFunctionDefinitionResult | null | undefined,
): null | {
  url: To;
  tableName: string;
} => {
  const table =
    func.configuration?.response?.type === 'table' &&
    func.configuration.response.table
      ? func.configuration.response.table
      : functionDefinition?.returnTable;

  if (!table) {
    return null;
  }

  return {
    url: dataRoutes.manageTable(source, table, 'permissions'),
    tableName: getTableDisplayName(table),
  };
};

type FunctionPermissionsProps = {
  source: Source;
  currentFunction: MetadataFunction;
  functionDefinition: GetFunctionDefinitionResult | null | undefined;
};

const FunctionPermissions = ({
  source,
  currentFunction,
  functionDefinition,
}: FunctionPermissionsProps) => {
  const { data: serverConfig } = useServerConfig();

  const isPermissionsEditable = useMemo(() => {
    const isFunctionExposedAsMutation =
      currentFunction.configuration?.exposed_as === 'mutation';

    if (
      functionDefinition?.isVolatile &&
      (isFunctionExposedAsMutation ||
        !serverConfig?.is_function_permissions_inferred)
    ) {
      return true;
    }

    return !serverConfig?.is_function_permissions_inferred;
  }, [functionDefinition, serverConfig]);

  const permissionTableURL = getRefTablePermissionsRoute(
    source.name,
    currentFunction,
    functionDefinition,
  );

  return (
    <Analytics name="CustomFunctionPermissions">
      <div className="mt-4">
        {permissionTableURL ? (
          <Text as="div">
            Permissions will be inherited from the SELECT permissions of the
            referenced table (
            <RelativeLink
              to={permissionTableURL.url}
              data-test="custom-function-permission-link"
            >
              <b>{permissionTableURL.tableName}</b>
            </RelativeLink>
            ) by default.
          </Text>
        ) : null}
        <div className="my-4">
          <PermissionServerFlagNote isEditable={isPermissionsEditable} />
        </div>
        <PermissionsEditor
          currentFunction={currentFunction}
          tableName={permissionTableURL?.tableName}
          isPermissionsEditable={isPermissionsEditable}
        />
      </div>
    </Analytics>
  );
};

export default FunctionPermissions;
