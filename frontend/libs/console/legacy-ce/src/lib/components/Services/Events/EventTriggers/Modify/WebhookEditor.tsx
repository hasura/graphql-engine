import React from 'react';
import { FaShieldAlt } from 'react-icons/fa';
import Editor from '../../../../Common/Layout/ExpandableEditor/Editor';
import { inputStyles } from '../../constants';
import { EventTrigger, URLConf, VoidCallback } from '../../types';
import { parseServerWebhook } from '../../utils';
import FormLabel from '../Add/FormLabel';
import DebouncedDropdownInput from '../Common/DropdownWrapper';

type WebhookEditorProps = {
  currentTrigger: EventTrigger;
  webhook: URLConf;
  setWebhook: (w: URLConf) => void;
  save: (success: VoidCallback, error: VoidCallback) => void;
};

const WebhookEditor = (props: WebhookEditorProps) => {
  const { currentTrigger, webhook, setWebhook, save } = props;
  const existingWebhook = parseServerWebhook(
    currentTrigger.configuration.webhook,
    currentTrigger.configuration.webhook_from_env
  );

  const reset = () => {
    setWebhook(existingWebhook);
  };

  const handleWebhookTypeChange = (e: React.BaseSyntheticEvent) => {
    const type = e.target.getAttribute('value');
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

  const collapsed = () => (
    <>
      <p>
        {existingWebhook.value}
        &nbsp;
      </p>
      <i>{existingWebhook.type === 'env' && '- from env'}</i>
    </>
  );

  const expanded = () => (
    <div className="w-1/2">
      <p className="text-sm text-gray-600 mb-sm">
        Note: Provide an URL or use an env var to template the handler URL if
        you have different URLs for multiple environments.
      </p>
      {existingWebhook?.type === 'env' ? (
        <DebouncedDropdownInput
          dropdownOptions={[
            { display_text: 'URL', value: 'static' },
            { display_text: 'From env var', value: 'env' },
          ]}
          title={webhook.type === 'env' ? 'From env var' : 'URL'}
          dataKey={webhook.type === 'env' ? 'env' : 'static'}
          onButtonChange={handleWebhookTypeChange}
          onHandlerValChange={handleWebhookValueChange}
          required
          bsClass={`w-82`}
          handlerVal={webhook.value}
          id="webhook-url"
          inputPlaceHolder={
            webhook.type === 'env'
              ? 'MY_WEBHOOK_URL'
              : 'http://httpbin.org/post'
          }
          testId="webhook"
        />
      ) : (
        <input
          type="text"
          name="handler"
          onChange={e => handleWebhookValueChange(e.target.value)}
          required
          className={`${inputStyles} w-82`}
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
    <div className="w-full border-b border-solid border-gray-300 mb-md">
      <div className="mb-md">
        <FormLabel
          title="Webhook (HTTP/S) Handler"
          tooltip="Environment variables and secrets are available using the {{VARIABLE}} tag. Environment variable templating is available for this field. Example: https://{{ENV_VAR}}/endpoint_url"
          tooltipIcon={
            <FaShieldAlt className="h-4 text-muted cursor-pointer" />
          }
          knowMoreLink="https://hasura.io/docs/latest/api-reference/syntax-defs/#webhookurl"
        />
        <Editor
          editorCollapsed={collapsed}
          editorExpanded={expanded}
          expandCallback={reset}
          property="webhook"
          service="modify-trigger"
          saveFunc={save}
        />
      </div>
    </div>
  );
};

export default WebhookEditor;
