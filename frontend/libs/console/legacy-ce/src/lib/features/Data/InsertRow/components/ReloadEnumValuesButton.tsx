import { useState } from 'react';
import { useQueryClient } from '@tanstack/react-query';
import { Flex } from '@radix-ui/themes';
import { Button, IconTooltip, hasuraToast } from '@hasura/shared/ui';
import { useErrorNotification, useReloadMetadata } from '@hasura/metadata/api';
import { invalidateEnumColumnValues } from '@hasura/metadata/data-source';

const TOOLTIP_TEXT =
  'Reload enum values in your GraphQL schema after inserting, updating or deleting enum values';

/**
 * Reloads the source of an enum table, so the rows changed in the table are
 * reflected in the GraphQL enum type and in the forms referencing it.
 */
export const ReloadEnumValuesButton = ({
  sourceName,
}: {
  sourceName: string;
}) => {
  const { reloadMetadata } = useReloadMetadata();
  const queryClient = useQueryClient();
  const showErrorNotification = useErrorNotification();
  const [isReloading, setIsReloading] = useState(false);

  const onReload = () => {
    setIsReloading(true);
    reloadMetadata({ sources: [sourceName] })
      .then(() => {
        void invalidateEnumColumnValues(queryClient, sourceName);
        hasuraToast({ type: 'success', title: 'Enum values reloaded' });
      })
      .catch((error) => {
        showErrorNotification({ title: 'Error reloading enum values', error });
      })
      .finally(() => setIsReloading(false));
  };

  return (
    <Flex align="center" gap="2">
      <Button
        type="button"
        mode="default"
        loading={isReloading}
        onClick={onReload}
      >
        Reload enum values
      </Button>
      <IconTooltip message={TOOLTIP_TEXT} />
    </Flex>
  );
};
