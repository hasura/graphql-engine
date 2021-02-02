import React from 'react';
import Editor from '../../../../Common/Layout/ExpandableEditor/Editor';
import DropdownButton from '../../../../Common/DropdownButton/DropdownButton';
import { EventTrigger, URLConf, VoidCallback } from '../../types';
import Tooltip from '../../../../Common/Tooltip/Tooltip';
import { parseServerWebhook } from '../../utils';

type WebhookEditorProps = {
  currentTrigger: EventTrigger;
  webhook: URLConf;
  setWebhook: (w: URLConf) => void;
  save: (success: VoidCallback, error: VoidCallback) => void;
  styles: Record<string, string>;
};

const WebhookEditor = (props: WebhookEditorProps) => {
  const { currentTrigger, webhook, setWebhook, save, styles } = props;

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

  const handleWebhookValueChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    const value = e.target.value;
    setWebhook({
      type: webhook.type,
      value,
    });
  };

  const collapsed = () => (
    <div className={styles.modifyProperty}>
      <p>
        {existingWebhook.value}
        &nbsp;
      </p>
      <i>{existingWebhook.type === 'env' && '- from env'}</i>
    </div>
  );

  const expanded = () => (
    <div className={styles.modifyWhDropdownWrapper}>
      <DropdownButton
        dropdownOptions={[
          { display_text: 'URL', value: 'static' },
          { display_text: 'From env var', value: 'env' },
        ]}
        title={webhook.type === 'env' ? 'From env var' : 'URL'}
        dataKey={webhook.type === 'env' ? 'env' : 'static'}
        onButtonChange={handleWebhookTypeChange}
        onInputChange={handleWebhookValueChange}
        required
        bsClass={styles.dropdown_button}
        inputVal={webhook.value}
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
    <div className={`${styles.container} ${styles.borderBottom}`}>
      <div className={styles.modifySection}>
        <h4 className={styles.modifySectionHeading}>
          Webhook URL <Tooltip message="Edit your webhook URL" />
        </h4>
        <Editor
          editorCollapsed={collapsed}
          editorExpanded={expanded}
          expandCallback={reset}
          property="webhook"
          service="modify-trigger"
          styles={styles}
          saveFunc={save}
        />
      </div>
    </div>
  );
};

export default WebhookEditor;
