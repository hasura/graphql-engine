import React, { useEffect, useState } from 'react';
import { useDebouncedEffect } from '@hasura/shared/hooks';
import { Analytics, REDACT_EVERYTHING } from '@hasura/shared/analytics';
import { FaShieldAlt } from 'react-icons/fa';
import { IconTooltip, Input, LearnMoreLink, Text } from '@hasura/shared/ui';
import { Flex } from '@radix-ui/themes';

const editorLabel = 'Webhook (HTTP/S) Handler';

type HandlerEditorProps = {
  value: string;
  onChange: (v: string) => void;
  disabled: boolean;
};

const HandlerEditor: React.FC<HandlerEditorProps> = ({
  value,
  onChange,
  disabled = false,
}) => {
  const [localValue, setLocalValue] = useState<string>(value);

  useEffect(() => {
    setLocalValue(value);
  }, [value]);

  useDebouncedEffect(
    () => {
      onChange(localValue);
    },
    1000,
    [localValue],
  );

  return (
    <Analytics name="ActionEditor" {...REDACT_EVERYTHING}>
      <div className="mb-6 w-6/12">
        <Flex align="center" gap="1" asChild>
          <Text size="3" weight="medium">
            {editorLabel}
            <Text color="red">*</Text>
            <IconTooltip
              message="Environment variables and secrets are available using the {{VARIABLE}} tag. Environment variable templating is available for this field. Example: https://{{ENV_VAR}}/endpoint_url"
              icon={<FaShieldAlt className="h-4 text-muted cursor-pointer" />}
            />
            <LearnMoreLink href="https://hasura.io/docs/latest/api-reference/syntax-defs/#webhookurl" />
          </Text>
        </Flex>
        <div className="mb-2">
          <Text size="1">
            Note: Provide an URL or use an env var to template the handler URL
            if you have different URLs for multiple environments.
          </Text>
        </div>
        <Input
          disabled={disabled}
          type="text"
          name="handler"
          value={localValue}
          onChange={(e) => setLocalValue(e.target.value)}
          placeholder="http://custom-logic.com/api or {{ACTION_BASE_URL}}/handler"
          data-test="action-create-handler-input"
        />
      </div>
    </Analytics>
  );
};

export default HandlerEditor;
