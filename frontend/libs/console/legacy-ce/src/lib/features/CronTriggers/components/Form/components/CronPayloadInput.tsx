import React from 'react';
import { CodeEditorField } from '../../../../../new-components/Form';
import { IAceOptions } from 'react-ace';

export const CronPayloadInput = () => {
  const editorOptions: IAceOptions = {
    fontSize: 14,
    showGutter: true,
    tabSize: 2,
    showLineNumbers: true,
    minLines: 10,
    maxLines: 10,
  };

  return (
    <CodeEditorField
      name="payload"
      label="Payload"
      tooltip="The request payload for the cron trigger, should be a valid JSON"
      editorOptions={editorOptions}
      theme="eclipse"
    />
  );
};
