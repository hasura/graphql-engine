import React from 'react';

import Editor from '../../../Common/Layout/ExpandableEditor/Editor';
import DropdownButton from '../../../Common/DropdownButton/DropdownButton';
import {
  setWebhookUrl,
  setWebhookUrlType,
  showValidationError,
} from './Actions';
import { ToolTip, Heading, Text } from '../../../UIKit/atoms';

const WebhookEditor = props => {
  const {
    webhook,
    env,
    dispatch,
    modifyTrigger,
    styles,
    save: saveWebhook,
  } = props;

  const setValues = () => {
    dispatch(setWebhookUrl(webhook));
    dispatch(setWebhookUrlType(env ? 'env' : 'url'));
  };

  const handleSelectionChange = e => {
    dispatch(setWebhookUrlType(e.target.getAttribute('value')));
    dispatch(setWebhookUrl(''));
  };

  // eslint-disable-next-line @typescript-eslint/no-unused-vars
  const validateAndSave = () => {
    if (modifyTrigger.webhookUrlType === 'url') {
      let tempUrl = false;
      try {
        tempUrl = new URL(modifyTrigger.webhookURL);
      } catch (e) {
        console.error(e);
      }
      if (!tempUrl) {
        dispatch(showValidationError('Invalid URL'));
        return;
      }
    }

    props.save();
  };

  const collapsed = () => (
    <div className={styles.modifyProperty}>
      <Text mr="xs" fontSize="button">
        {webhook}
      </Text>
      <i>{env && '- from env'}</i>
    </div>
  );

  const expanded = () => (
    <div className={styles.modifyWhDropdownWrapper}>
      <DropdownButton
        dropdownOptions={[
          { display_text: 'URL', value: 'url' },
          { display_text: 'From env var', value: 'env' },
        ]}
        title={modifyTrigger.webhookUrlType === 'env' ? 'From env var' : 'URL'}
        dataKey={modifyTrigger.webhookUrlType === 'env' ? 'env' : 'url'}
        onButtonChange={handleSelectionChange}
        onInputChange={e => dispatch(setWebhookUrl(e.target.value))}
        required
        bsClass={styles.dropdown_button}
        inputVal={modifyTrigger.webhookURL}
        id="webhook-url"
        inputPlaceHolder={
          modifyTrigger.webhookUrlType === 'env'
            ? 'MY_WEBHOOK_URL'
            : 'http://httpbin.org/post'
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
        <Heading as="h4" fontSize="15px" mb="20px">
          Webhook URL <ToolTip message="Edit your webhook URL" ml="sm" />
        </Heading>
        <Editor
          editorCollapsed={collapsed}
          editorExpanded={expanded}
          expandCallback={setValues}
          property="webhook"
          service="modify-trigger"
          ongoingRequest={modifyTrigger.ongoingRequest}
          styles={styles}
          saveFunc={saveWebhook}
        />
      </div>
    </div>
  );
};

export default WebhookEditor;
