import React from 'react';
import Editor from '../../../../Common/Layout/ExpandableEditor/Editor';
import { inputStyles } from '../../constants';
import { EventTrigger, URLConf, VoidCallback } from '../../types';
import { parseServerWebhook } from '../../utils';
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
    <div className="pb-sm pt-sm max-w-80">
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
        bsClass={`${inputStyles} w-72`}
        handlerVal={webhook.value}
        id="webhook-url"
        inputPlaceHolder={
          webhook.type === 'env' ? 'MY_WEBHOOK_URL' : 'http://httpbin.org/post'
        }
        testId="webhook"
      />
      <br />
      <small>
        Note: Specifying the webhook URL via an environmental variable is
        recommended if you have different URLs for multiple environments.
      </small>
    </div>
  );

  return (
    <div className="w-full border-b border-solid border-gray-300 mb-md">
      <div className="mb-md">
        <h4 className="text-md font-bold mb-md">Webhook (HTTP/S) Handler</h4>
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
