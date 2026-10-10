import React from 'react';
import { Checkbox, HeadersInput, Text } from '@hasura/shared/ui';
import { ClientHeader } from '@hasura/shared/types';
import { Heading } from '@radix-ui/themes';

const editorLabel = 'Headers';
const editorSecText =
  'Headers Hasura will send to the webhook with the POST request.';

type HeaderConfEditorProps = {
  forwardClientHeaders: boolean;
  toggleForwardClientHeaders: (value: boolean) => void;
  headers: ClientHeader[];
  setHeaders: (hs: ClientHeader[]) => void;
  disabled?: boolean;
};

const HeaderConfEditor: React.FC<HeaderConfEditorProps> = ({
  forwardClientHeaders,
  toggleForwardClientHeaders,
  headers,
  setHeaders,
  disabled = false,
}) => {
  return (
    <>
      <Heading size="3">{editorLabel}</Heading>
      <Text as="p" size="1">
        {editorSecText}
      </Text>

      <div className="my-4">
        <Checkbox
          name="checkbox"
          value={forwardClientHeaders}
          onChange={toggleForwardClientHeaders}
          disabled={disabled}
        >
          Forward client headers to webhook
        </Checkbox>
      </div>
      <HeadersInput
        headers={headers}
        setHeaders={setHeaders}
        disabled={disabled}
      />
    </>
  );
};

export default HeaderConfEditor;
