import { useState } from 'react';
import { Button } from '@hasura/shared/ui';
import { getConfirmation } from '@hasura/shared/utils';
import { useResetMetadata } from '@hasura/metadata/api';

const ResetMetadata = () => {
  const [isResetting, setIsResetting] = useState(false);
  const resetMetadata = useResetMetadata();

  const handleReset = (e) => {
    e.preventDefault();
    const confirmMessage =
      'This will permanently reset the Hasura metadata related to your tables, remote schemas, actions and triggers';
    const isOk = getConfirmation(confirmMessage, true);
    if (!isOk) {
      return;
    }

    const completionCallback = () => setIsResetting(false);
    setIsResetting(true);
    resetMetadata().finally(completionCallback);
  };

  return (
    <div className="inline-block">
      <Button
        data-test="data-reset-metadata"
        loading={isResetting}
        loadingText="Resetting..."
        mode="default"
        onClick={handleReset}
        size="2"
      >
        Reset
      </Button>
    </div>
  );
};

export default ResetMetadata;
