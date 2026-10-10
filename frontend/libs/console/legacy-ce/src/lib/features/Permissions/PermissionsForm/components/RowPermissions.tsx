import React from 'react';
import { useFormContext } from 'react-hook-form';
import { useQuery } from '@tanstack/react-query';
import {
  InputField,
  IconTooltip,
  Collapsible,
  CollapsibleHeader,
  Radio,
  Text,
  AceEditor,
} from '@hasura/shared/ui';
import { getDatabaseMethods, Operator } from '@hasura/metadata/data-source';
import { getTypeName, getIngForm } from '@hasura/shared/utils';
import {
  DataQueryType,
  Metadata,
  MetadataTable,
  Source,
} from '@hasura/shared/types';
import { copyQueryTypePermissions } from '../utils/copyQueryTypePermissions';
import { getNonSelectedQueryTypePermissions } from '../utils/getMapQueryTypePermissions';
import { RowPermissionBuilder } from './RowPermissionsBuilder';
import { Flex, Skeleton, Strong } from '@radix-ui/themes';

const NoChecksLabel = () => (
  <span data-test="without-checks">Without any checks&nbsp;</span>
);

const CustomLabel = () => (
  <Flex data-testid="custom-check" align="center" gap="2">
    With custom check:
    <IconTooltip message="Create custom check using permissions builder" />
  </Flex>
);

export interface RowPermissionsProps {
  metadata: Metadata['metadata'];
  table: MetadataTable;
  queryType: DataQueryType;
  subQueryType?: 'pre_update' | 'post_update';
  source: Source;
  supportedOperators: Operator[];
  defaultValues: Record<string, any>;
  permissionsKey: 'check' | 'filter';
  roleName: string;
}

enum SelectedSection {
  NoChecks = 'no_checks',
  Custom = 'custom',
  NoneSelected = 'none',
  insert = 'insert',
  select = 'select',
  update = 'update',
  delete = 'delete',
}

export type RowPermissionsSectionType =
  DataQueryType | 'pre_update' | 'post_update';

export const getRowPermission = (
  queryType: RowPermissionsSectionType,
): 'filter' | 'check' => {
  if (queryType === 'pre_update') {
    return 'filter';
  }
  if (queryType === 'post_update') {
    return 'check';
  }
  if (queryType === 'insert') {
    return 'check';
  }

  return 'filter';
};

const getRowPermissionCheckType = (
  queryType: RowPermissionsSectionType,
): 'filterType' | 'checkType' => {
  const permissionType = getRowPermission(queryType);
  return permissionType === 'filter' ? 'filterType' : 'checkType';
};

const useTypeName = ({
  table,
  source,
}: {
  table: MetadataTable;
  source: Source;
}) => {
  return useQuery({
    queryKey: [source.name, 'gql_introspection', 'type_name', table],
    queryFn: async () => {
      const databaseMethods = getDatabaseMethods(source.kind);

      const defaultQueryRoot = databaseMethods.config.getDefaultQueryRoot({
        table: table.table,
      });

      // This is very GDC specific. We have to move this to DAL later
      const typeName = getTypeName({
        defaultQueryRoot,
        operation: 'select',
        sourceCustomization: source.customization,
        configuration: table.configuration,
      });

      return typeName;
    },
  });
};

const getUpdatePermissionBuilderIdSuffix = (
  id: string,
  subQueryType: string | undefined,
) => {
  if (subQueryType) return `${id}-${subQueryType}`;
  return id;
};

export const RowPermissionsSection: React.FC<RowPermissionsProps> = ({
  table: metadataTable,
  queryType,
  subQueryType,
  source,
  defaultValues,
  permissionsKey,
  roleName,
  metadata,
}) => {
  const { data: tableName, isLoading } = useTypeName({
    table: metadataTable,
    source,
  });

  const nonSelectedQueryTypePermissions = getNonSelectedQueryTypePermissions(
    metadataTable,
    queryType,
    roleName,
  );

  const { watch, setValue } = useFormContext();
  // determines whether the inputs should be pointed at `check` or `filter`
  const rowPermissions = getRowPermission(subQueryType ?? queryType);

  // determines whether the check type should be pointed at `checkType` or `filterType`
  const rowPermissionsCheckType = getRowPermissionCheckType(
    subQueryType ?? queryType,
  );

  const selectedSection = watch(rowPermissionsCheckType);

  return (
    <fieldset key={queryType} className="grid gap-2">
      <div>
        <Radio
          id={SelectedSection.NoChecks}
          value={SelectedSection.NoChecks}
          checked={selectedSection === SelectedSection.NoChecks}
          onClick={() => {
            setValue(rowPermissionsCheckType, SelectedSection.NoChecks);
            setValue(rowPermissions, {});
          }}
        >
          <NoChecksLabel />
        </Radio>
      </div>

      {nonSelectedQueryTypePermissions &&
        nonSelectedQueryTypePermissions?.map(
          ({ queryType: type, data }: Record<string, any>) => (
            <div key={`${type}${queryType}`}>
              <Radio
                id={`custom_${type}`}
                data-testid={getUpdatePermissionBuilderIdSuffix(
                  `external-${roleName}-${type}-input`,
                  subQueryType,
                )}
                value={type}
                checked={selectedSection === type}
                onClick={() => {
                  setValue(rowPermissionsCheckType, type);
                  const newValues = copyQueryTypePermissions(
                    type,
                    queryType,
                    subQueryType,
                    data,
                  );
                  setValue(...newValues);
                }}
              >
                <span data-test="mutual-check">
                  With same custom check as&nbsp;<strong>{type}</strong>
                </span>
              </Radio>

              {selectedSection === type && (
                <div
                  // Permissions are not otherwise stored in plan JSON format in the dom.
                  // This is a hack to get the JSON into the dom for testing.
                  data-state={JSON.stringify(
                    getRowPermission(type)
                      ? data?.[getRowPermission(type)]
                      : {},
                  )}
                  data-testid="external-check-json-editor"
                  className="mt-4 p-6 min-h-32 w-full"
                >
                  <AceEditor
                    mode="json"
                    minLines={1}
                    fontSize={14}
                    height="18px"
                    width="100%"
                    name={`${tableName}-json-editor`}
                    value={JSON.stringify(data[getRowPermission(type)])}
                    onChange={() => setValue(rowPermissionsCheckType, type)}
                    editorProps={{ $blockScrolling: true }}
                  />
                </div>
              )}
            </div>
          ),
        )}

      <div>
        <Radio
          id={SelectedSection.Custom}
          data-testid={getUpdatePermissionBuilderIdSuffix(
            `custom-${roleName}-${queryType}-input`,
            subQueryType,
          )}
          value={SelectedSection.Custom}
          checked={selectedSection === SelectedSection.Custom}
          onClick={() => {
            setValue(rowPermissionsCheckType, SelectedSection.Custom);
            // eslint-disable-next-line @typescript-eslint/ban-ts-comment
            // @ts-ignore
            // problem with inferring other types than select which does not have 'check'
            setValue(rowPermissions, defaultValues[rowPermissions]);
          }}
        >
          <CustomLabel />
        </Radio>

        {selectedSection === SelectedSection.Custom && (
          <div className="pt-4">
            {!isLoading && tableName ? (
              <RowPermissionBuilder
                metadata={metadata}
                permissionsKey={permissionsKey}
                table={metadataTable.table}
                source={source}
              />
            ) : (
              <Skeleton height="1rem" width="100%" />
            )}
          </div>
        )}
      </div>

      {queryType === 'select' && (
        <div className="sm:w-1/2">
          <InputField
            label="Limit number of rows"
            name="rowCount"
            noErrorPlaceholder
          />
        </div>
      )}
    </fieldset>
  );
};

export interface RowPermissionsWrapperProps {
  queryType: DataQueryType;
  roleName: string;
  defaultOpen?: boolean;
  children?: React.ReactNode;
}

const getStatus = (rowPermissions: string) => {
  if (!rowPermissions) {
    return 'No access';
  }

  if (rowPermissions === '{}') {
    return 'Without any checks';
  }

  return 'With custom checks';
};

export const RowPermissionsSectionWrapper: React.FC<
  RowPermissionsWrapperProps
> = ({ children, queryType, roleName, defaultOpen }) => {
  const { watch } = useFormContext();

  const rowPermissions = watch('rowPermissions');
  const status = getStatus(rowPermissions);

  return (
    <div className="mt-4">
      <Collapsible
        defaultOpen={defaultOpen}
        data-test="toggle-row-permission"
        triggerChildren={
          <CollapsibleHeader
            title={`Row ${queryType} permissions`}
            tooltip={`Set permission rule for ${getIngForm(queryType)} rows`}
            status={status}
          />
        }
      >
        <div className="mb-2">
          <Text>
            Allow role <Strong>{roleName}</Strong> to {queryType}&nbsp;
            <Strong>rows</Strong>:
          </Text>
        </div>
        {children}
      </Collapsible>
    </div>
  );
};

export default RowPermissionsSection;
