import {
  MetadataSelectors,
  useMetadata,
} from '../../../../hasura-metadata-api';
import { Button } from '../../../../../new-components/Button';
import { Dialog } from '../../../../../new-components/Dialog';
import { InputField, SimpleForm } from '../../../../../new-components/Form';
import { sanitizeGraphQLFieldNames } from '../../../../../utils';
import { SanitizeTips } from '../../../../../utils/sanitizeGraphQLFieldNames';
import React from 'react';
import { useUpdateTableConfiguration } from '../../../ModifyTable/hooks';
import { ModifyTableProps } from '../../ModifyTable';
import { ModifyTableColumn } from '../../types';
import { schema, Schema } from './schema';

interface EditTableColumnDialogProps extends ModifyTableProps {
  onClose: () => void;
  column: ModifyTableColumn;
}

export const EditTableColumnDialog = (props: EditTableColumnDialogProps) => {
  const { onClose, column, table, dataSourceName } = props;

  const { data: metadataTable } = useMetadata(
    MetadataSelectors.findTable(dataSourceName, table)
  );

  const { isLoading: isSaveInProgress, updateTableConfiguration } =
    useUpdateTableConfiguration(dataSourceName, table);

  return (
    <SimpleForm
      schema={schema}
      onSubmit={(data: Schema) => {
        updateTableConfiguration({
          column_config: {
            ...metadataTable?.configuration?.column_config,
            [column.name]: data,
          },
          custom_column_names: {
            ...metadataTable?.configuration?.custom_column_names,
            [column.name]: data.custom_name,
          },
        }).then(() => {
          onClose();
        });
      }}
      options={{
        defaultValues: {
          custom_name: column.config?.custom_name ?? '',
          comment: column.config?.comment ?? '',
        },
      }}
    >
      <Dialog
        size="md"
        titleTooltip={`Edit ${column.name} column settings`}
        title={`[${column.name}]`}
        hasBackdrop
        onClose={onClose}
        footer={
          <div className="bg-white p-2 justify-end border flex">
            <Button type="submit" isLoading={isSaveInProgress}>
              Submit
            </Button>
          </div>
        }
      >
        <div className="m-4">
          <SanitizeTips />
          <InputField
            label="Custom GraphQL Field Name"
            name="custom_name"
            placeholder="Enter GraphQL Field Name"
            tooltip="Add a custom GQL field name for table column"
            inputTransform={val => sanitizeGraphQLFieldNames(val)}
          />
          <InputField
            tooltip="Add a comment for your table column"
            label="Comment"
            name="comment"
            placeholder="Add a comment"
          />
        </div>
      </Dialog>
    </SimpleForm>
  );
};
