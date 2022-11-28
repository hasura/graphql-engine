import { TableTrackingCustomizationModal } from '@/components/Services/Data/Schema/tableTrackCustomization/TableTrackingCustomizationModal';
import { Button } from '@/new-components/Button';
import React from 'react';
import { useMetadataTable, useUpdateTableConfiguration } from '../hooks';
import { ModifyTableProps } from '../ModifyTable';

const RootField: React.VFC<{ property: string; value: string }> = ({
  property,
  value,
}) => (
  <div className="mb-2">
    <strong>{property}</strong>
    <span className="mx-2">→</span>
    <span>{value}</span>
  </div>
);

export const TableRootFields: React.VFC<ModifyTableProps> = props => {
  const { dataSourceName, table, tableName } = props;
  const [showCustomModal, setShowCustomModal] = React.useState(false);
  const { metadataTable, isLoading } = useMetadataTable(dataSourceName, table);

  const { updateCustomRootFields, isLoading: saving } =
    useUpdateTableConfiguration(dataSourceName, table);

  if (isLoading) {
    return null;
  }

  const isEmpty =
    !metadataTable?.configuration?.custom_name &&
    (!metadataTable?.configuration?.custom_root_fields ||
      Object.keys(metadataTable?.configuration?.custom_root_fields).length ===
        0);

  return (
    <div>
      <Button onClick={() => setShowCustomModal(true)} className="mb-2">
        {isEmpty ? 'Add Custom Field Names' : 'Edit Custom Field Names'}
      </Button>
      <div className="p-2">
        {isEmpty && (
          <div className="text-gray-500 mx-2">
            No custom fields are currently set.
          </div>
        )}
        {metadataTable?.configuration?.custom_name && (
          <RootField
            property="custom_table_name"
            value={metadataTable.configuration.custom_name}
          />
        )}
        {Object.entries(
          metadataTable?.configuration?.custom_root_fields ?? {}
        ).map(([key, value]) => (
          <RootField key={key} property={key} value={value} />
        ))}
      </div>
      <TableTrackingCustomizationModal
        tableName={tableName}
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
        currentConfiguration={metadataTable?.configuration}
      />
    </div>
  );
};
