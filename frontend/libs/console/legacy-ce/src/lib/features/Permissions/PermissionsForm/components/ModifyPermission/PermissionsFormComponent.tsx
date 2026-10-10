import React from 'react';
import { Flex, Strong } from '@radix-ui/themes';
import {
  useConsoleForm,
  Button,
  Text,
  TextAreaField,
  useDestructiveConfirm,
} from '@hasura/shared/ui';
import { PermissionsSchema, schema } from '../../../schema';
import ColumnRootFieldPermissions from '../RootFieldPermissions/RootFieldPermissions';
import {
  createFormData,
  getAllowedFilterKeys,
} from '../../hooks/dataFetchingHooks/useFormData/createFormData/index';
import { InputValidation } from '../InputValidation/InputValidation';
import { useDropTablePermission } from '@hasura/metadata/api';
import {
  AccessType,
  DataQueryType,
  Metadata,
  MetadataTable,
  Source,
} from '@hasura/shared/types';
import { useSubmitForm } from '../../hooks/submitHooks/useSubmitForm';
import { FaRegComment } from 'react-icons/fa6';
import RowPermissionsSection, {
  RowPermissionsSectionWrapper,
} from '../RowPermissions';
import ColumnPermissionsSection from '../ColumnPermissions';
import ColumnPresetsSection from '../ColumnPresets';
import AggregationSection from '../Aggregation';
import BackendOnlySection from '../BackendOnly';

export interface ComponentProps {
  metadata: Metadata['metadata'];
  dataSource: Source;
  table: MetadataTable;
  queryType: DataQueryType;
  roleName: string;
  accessType: AccessType;
  handleClose: () => void;
  defaultValues: PermissionsSchema;
  formData: ReturnType<typeof createFormData>;
  showCloseButton?: boolean;
}

const PermissionsFormComponent = ({
  metadata,
  dataSource,
  table: metadataTable,
  queryType,
  roleName,
  accessType,
  handleClose,
  defaultValues,
  formData,
  showCloseButton,
}: ComponentProps) => {
  // functions fired when the form is submitted
  const { dropTablePermission, isPending: isDroppingTable } =
    useDropTablePermission();
  const destructiveConfirm = useDestructiveConfirm();

  const { submit, isPending: formLoading } = useSubmitForm({
    dataSourceName: dataSource.name,
    table: metadataTable.table,
    queryType,
    roleName,
    accessType,
    validateInput: defaultValues?.validateInput,
  });

  const onSubmit = async ({ validateInput, ...rest }: PermissionsSchema) => {
    const _validateInput = validateInput?.enabled ? validateInput : undefined;

    if (_validateInput?.definition && !_validateInput.definition.timeout) {
      _validateInput.definition.timeout = 10;
    }

    submit(
      {
        ...rest,
        validateInput: _validateInput,
      },
      {
        onSuccess: () => {
          handleClose();
        },
      },
    );
  };

  const handleDelete = async () => {
    destructiveConfirm({
      resourceName: `${roleName} / ${queryType}`,
      resourceType: 'Permission',
      destroyTerm: 'delete',
      onConfirm: async () => {
        return new Promise((resolve) => {
          return dropTablePermission(
            {
              table: metadataTable.table,
              operation: queryType,
              role: roleName,
              source: dataSource,
            },
            {
              onSuccess: () => {
                resolve(true);
                handleClose();
              },
              onError: () => resolve(false),
            },
          );
        });
      },
    });
  };

  const {
    methods: { getValues },
    Form,
  } = useConsoleForm({
    schema,
    options: {
      defaultValues,
    },
  });

  const filterType = getValues('filterType');
  const checkType = getValues('checkType');
  const filterKeys = getAllowedFilterKeys(queryType);

  return (
    <Form onSubmit={onSubmit}>
      <div className="md:w-10/12 p-4">
        <Flex align="center" gap="4" className="pb-4">
          {Boolean(showCloseButton) && (
            <Button size="1" mode="default" type="button" onClick={handleClose}>
              Close
            </Button>
          )}
          <Text data-testid="form-title">
            <Strong>Role:</Strong> {roleName} <Strong>Action:</Strong>{' '}
            {queryType}
          </Text>
        </Flex>
        <div className="mb-4">
          <TextAreaField
            label="Comments"
            labelIcon={<FaRegComment />}
            name="comment"
            placeholder="Add a comment explaining the permissions"
            noErrorPlaceholder
          />
        </div>
        {queryType !== 'select' && (
          <InputValidation formFieldsNamePrefix="validateInput." />
        )}
        <RowPermissionsSectionWrapper
          roleName={roleName}
          queryType={queryType}
          defaultOpen
        >
          <React.Fragment>
            {queryType === 'update' && (
              <div className="my-2">
                <Text>
                  <Strong>Pre-update &nbsp; check</Strong>
                  &nbsp;
                </Text>
              </div>
            )}
            <RowPermissionsSection
              metadata={metadata}
              table={metadataTable}
              roleName={roleName}
              queryType={queryType}
              subQueryType={queryType === 'update' ? 'pre_update' : undefined}
              permissionsKey={filterKeys[0]}
              source={dataSource}
              supportedOperators={defaultValues?.supportedOperators ?? []}
              defaultValues={defaultValues}
            />
            {queryType === 'update' && (
              <div data-testid="post-update-check-container">
                <div className="my-2">
                  <Text as="div">
                    <Strong>Post-update &nbsp; check</Strong>
                    &nbsp; (optional)
                  </Text>
                </div>

                <RowPermissionsSection
                  metadata={metadata}
                  table={metadataTable}
                  roleName={roleName}
                  queryType={queryType}
                  subQueryType={
                    queryType === 'update' ? 'post_update' : undefined
                  }
                  permissionsKey={filterKeys[1]}
                  source={dataSource}
                  supportedOperators={defaultValues?.supportedOperators ?? []}
                  defaultValues={defaultValues}
                />
              </div>
            )}
          </React.Fragment>
        </RowPermissionsSectionWrapper>
        {queryType !== 'delete' && (
          <ColumnPermissionsSection
            roleName={roleName}
            queryType={queryType}
            columns={formData?.columns}
            computedFields={formData?.computed_fields}
            table={metadataTable}
            source={dataSource}
          />
        )}
        {['insert', 'update'].includes(queryType) && (
          <div className="my-4">
            <ColumnPresetsSection
              queryType={queryType}
              columns={formData?.columns}
            />
          </div>
        )}
        {queryType === 'select' && (
          <>
            <AggregationSection
              dataSource={dataSource}
              queryType={queryType}
              roleName={roleName}
            />
            <ColumnRootFieldPermissions
              filterType={filterType}
              source={dataSource}
              table={metadataTable.table}
            />
          </>
        )}
        {['insert', 'update', 'delete'].includes(queryType) && (
          <BackendOnlySection queryType={queryType} />
        )}
        <Flex
          className="mt-4"
          id="form-buttons-container"
          gap="2"
          align="center"
        >
          <Button
            data-testid="permissions-form-submit"
            type="submit"
            mode="primary"
            title={
              (queryType === 'insert' && checkType === 'none') ||
              filterType === 'none'
                ? 'You must select an option for row permissions'
                : 'Submit'
            }
            loading={formLoading}
            disabled={formLoading || isDroppingTable}
          >
            Save Permissions
          </Button>

          {accessType !== 'noAccess' && (
            <Button
              type="button"
              disabled={formLoading || isDroppingTable}
              mode="destructive"
              loading={isDroppingTable}
              onClick={handleDelete}
            >
              Delete Permissions
            </Button>
          )}
        </Flex>
      </div>
    </Form>
  );
};

export default PermissionsFormComponent;
