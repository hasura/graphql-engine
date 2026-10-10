import { useState } from 'react';
import { Button, IconTooltip, Checkbox, hasuraToast } from '@hasura/shared/ui';
import { useErrorNotification, useReloadMetadata } from '@hasura/metadata/api';
import { Flex } from '@radix-ui/themes';

type Props = {
  buttonText?: string;
  btnTooltipMessage?: string;
  showReloadRemoteSchemas?: boolean;
};

const ReloadMetadata = ({
  buttonText: _buttonText,
  btnTooltipMessage,
  showReloadRemoteSchemas = true,
}: Props) => {
  const { reloadMetadata, isLoading } = useReloadMetadata();
  const showErrorNotification = useErrorNotification();
  const [shouldReloadRemoteSchemas, setShouldReloadRemoteSchemas] =
    useState(false);
  const [shouldReloadAllSources, setShouldReloadAllSources] = useState(false);

  const toggleShouldReloadRemoteSchemas = () => {
    setShouldReloadRemoteSchemas(!shouldReloadRemoteSchemas);
  };

  const toggleShouldReloadAllSources = () => {
    setShouldReloadAllSources(!shouldReloadAllSources);
  };

  const reloadMetadataAndLoadInconsistentMetadata = (e) => {
    e.preventDefault();

    reloadMetadata({
      shouldReloadRemoteSchemas,
      shouldReloadAllSources,
    })
      .then(() => {
        hasuraToast({
          type: 'success',
          title: 'Metadata reloaded',
        });
      })
      .catch((err) => {
        showErrorNotification({
          title: 'Error reloading metadata',
          error: err,
        });
      });
  };

  const buttonText = isLoading ? 'Reloading' : 'Reload';

  return (
    <Flex align="center" gap="6">
      <Button
        data-test="data-reload-metadata"
        disabled={isLoading}
        onClick={reloadMetadataAndLoadInconsistentMetadata}
        mode="default"
      >
        {_buttonText || buttonText}
      </Button>
      {btnTooltipMessage && <IconTooltip message={btnTooltipMessage} />}
      {showReloadRemoteSchemas && (
        <Flex align="center" gap="2">
          <Checkbox
            onChange={toggleShouldReloadRemoteSchemas}
            value={shouldReloadRemoteSchemas}
            disabled={isLoading}
          >
            Reload all remote schemas
          </Checkbox>
          <IconTooltip message="Check this if you have inconsistent remote schemas or if your remote schema has changed." />
        </Flex>
      )}
      <Flex align="center" gap="2">
        <Checkbox
          onChange={toggleShouldReloadAllSources}
          value={shouldReloadAllSources}
          disabled={isLoading}
        >
          Reload all databases
        </Checkbox>
        <IconTooltip message="Check this if you have inconsistent databases." />
      </Flex>
    </Flex>
  );
};

export default ReloadMetadata;
