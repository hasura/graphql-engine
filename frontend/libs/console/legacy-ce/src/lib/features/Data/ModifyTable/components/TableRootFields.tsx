import { CustomFieldNames } from '../..';
import { Button, Text } from '@hasura/shared/ui';
import { getTableDisplayName } from '@hasura/shared/utils';
import React from 'react';
import { useUpdateTableConfiguration } from '../hooks';
import { ModifyTableProps } from '../types';
import { Strong } from '@radix-ui/themes';

export const RootField: React.FC<{ property: string; value: string }> = ({
  property,
  value,
}) => (
  <div className="mb-2">
    <Text>
      <Strong>{property}</Strong>
      <span className="mx-2">→</span>
      <span>{value}</span>
    </Text>
  </div>
);

export const TableRootFields: React.FC<ModifyTableProps> = ({
  source,
  table,
}) => {
  const [showCustomModal, setShowCustomModal] = React.useState(false);

  const { updateCustomRootFields, isPending: saving } =
    useUpdateTableConfiguration(source.name, table.table);

  const isEmpty =
    !table.configuration?.custom_name &&
    (!table.configuration?.custom_root_fields ||
      Object.keys(table.configuration?.custom_root_fields).length === 0);

  return (
    <div>
      <Button
        mode="default"
        size="1"
        onClick={() => setShowCustomModal(true)}
        className="mb-2"
      >
        {isEmpty ? 'Add Custom Field Names' : 'Edit Custom Field Names'}
      </Button>
      <div className="p-2">
        {isEmpty && <Text>No custom fields are currently set.</Text>}
        {table.configuration?.custom_name && (
          <RootField
            property="custom_table_name"
            value={table.configuration.custom_name}
          />
        )}
        {Object.entries(table.configuration?.custom_root_fields ?? {}).map(
          ([key, value]) => (
            <RootField key={key} property={key} value={value} />
          ),
        )}
      </div>
      <CustomFieldNames.Modal
        tableName={getTableDisplayName(table.table)}
        onSubmit={(formValues, config) => {
          updateCustomRootFields(config).then(() => {
            setShowCustomModal(false);
          });
        }}
        onClose={() => {
          setShowCustomModal(false);
        }}
        isLoading={saving}
        dialogDescription=""
        show={showCustomModal}
        currentConfiguration={table.configuration}
        source={source.name}
      />
    </div>
  );
};
