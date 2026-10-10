import { useEffect, useState } from 'react';
import { FaShieldAlt } from 'react-icons/fa';
import {
  DropdownButton,
  DropdownMenu,
  ExpandableEditor,
  ExpandableEditorFunction,
  FieldLabel,
  Input,
  Text,
} from '@hasura/shared/ui';
import { useDebouncedEffect } from '@hasura/shared/hooks';
import type { URLConf, URLType } from '../../types';
import { parseServerWebhook } from '../../utils';
import { Em, Flex } from '@radix-ui/themes';
import { EventTrigger } from '@hasura/shared/types';

type WebhookEditorProps = {
  currentTrigger: EventTrigger;
  webhook: URLConf;
  setWebhook: (w: URLConf) => void;
  save: ExpandableEditorFunction;
};

const WebhookEditor = (props: WebhookEditorProps) => {
  const { currentTrigger, webhook, setWebhook, save } = props;
  const existingWebhook = parseServerWebhook(
    currentTrigger.webhook,
    currentTrigger.webhook_from_env,
  );

  const reset = () => {
    setWebhook(existingWebhook);
  };

  const handleWebhookTypeChange = (type: URLType) => {
    setWebhook({
      type,
      value: '',
    });
  };

  const handleWebhookValueChange = (value: string) => {
    setWebhook({
      type: webhook.type,
      value,
    });
  };

  const [localValue, setLocalValue] = useState<string>(webhook.value);

  useEffect(() => {
    setLocalValue(webhook.value);
  }, [webhook.value]);

  useDebouncedEffect(
    () => {
      handleWebhookValueChange(localValue);
    },
    1000,
    [localValue],
  );

  const collapsed = () => (
    <>
      <Text as="p">
        {existingWebhook.value}
        &nbsp;
      </Text>
      <Text>
        <Em>{existingWebhook.type === 'env' && '- from env'}</Em>
      </Text>
    </>
  );

  const expanded = () => (
    <div className="w-1/2">
      <div className="mb-2">
        <Text color="gray">
          Note: Provide an URL or use an env var to template the handler URL if
          you have different URLs for multiple environments.
        </Text>
      </div>
      {existingWebhook?.type === 'env' ? (
        <Flex className="w-82">
          <DropdownButton
            data-test="webhook-dropdown-button"
            items={[
              <DropdownMenu.Item
                key="static"
                data-test="webhook-dropdown-item-1"
                onSelect={() => handleWebhookTypeChange('static')}
              >
                URL
              </DropdownMenu.Item>,
              <DropdownMenu.Item
                key="env"
                data-test="webhook-dropdown-item-2"
                onSelect={() => handleWebhookTypeChange('env')}
              >
                From env var
              </DropdownMenu.Item>,
            ]}
          >
            {webhook.type === 'env' ? 'From env var' : 'URL'}
          </DropdownButton>
          <Input
            style={{ marginLeft: '-1px' }}
            type="text"
            required
            onChange={(e) => setLocalValue(e.target.value)}
            value={localValue || ''}
            placeholder={
              webhook.type === 'env'
                ? 'MY_WEBHOOK_URL'
                : 'http://httpbin.org/post'
            }
            id="webhook-url"
            data-test="webhook-input"
          />
        </Flex>
      ) : (
        <Input
          type="text"
          name="handler"
          onChange={(e) => handleWebhookValueChange(e.target.value)}
          required
          value={
            webhook.type === 'static' ? webhook.value : `{{${webhook.value}}}`
          }
          id="webhook-url"
          placeholder="http://httpbin.org/post or {{MY_WEBHOOK_URL}}/handler"
          data-test="webhook"
        />
      )}
      <br />
    </div>
  );

  return (
    <div className="mb-4">
      <FieldLabel
        label="Webhook (HTTP/S) Handler"
        tooltip="Environment variables and secrets are available using the {{VARIABLE}} tag. Environment variable templating is available for this field. Example: https://{{ENV_VAR}}/endpoint_url"
        tooltipIcon={<FaShieldAlt className="h-4 text-muted cursor-pointer" />}
        learnMoreLink="https://hasura.io/docs/latest/api-reference/syntax-defs/#webhookurl"
      />
      <ExpandableEditor
        editorCollapsed={collapsed}
        editorExpanded={expanded}
        expandCallback={reset}
        property="webhook"
        service="modify-trigger"
        saveFunc={save}
      />
    </div>
  );
};

export default WebhookEditor;
