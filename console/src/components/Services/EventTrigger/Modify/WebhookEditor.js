import React from 'react';
import Editor from './Editor';
import DropdownButton from '../../../Common/DropdownButton/DropdownButton';
import { setWebhookUrl, setWebhookUrlType } from './Actions';

class WebhookEditor extends React.Component {
  setValues = () => {
    const { webhook, env, dispatch } = this.props;
    dispatch(setWebhookUrl(webhook));
    dispatch(setWebhookUrlType(env ? 'env' : 'static'));
  };

  handleSelectionChange = e => {
    const { dispatch } = this.props;
    dispatch(setWebhookUrlType(e.target.getAttribute('value')));
    dispatch(setWebhookUrl(''));
  };

  render() {
    const { webhook, modifyTrigger, env, dispatch, save, styles } = this.props;
    const collapsed = toggleButton => (
      <div className={styles.modifyWebhookCollapsed}>
        {toggleButton('Edit')}
        <div className={styles.modifyProperty}>
          <b>{webhook}</b>
          <i>{env && ' - from env variable'}</i>
        </div>
      </div>
    );

    const expanded = (toggleButton, saveButton) => (
      <div className={styles.modifyWebhookExpanded}>
        {toggleButton('Close')}
        <div className={styles.modifyWhDropdownWrapper}>
          <DropdownButton
            dropdownOptions={[
              { display_text: 'URL', value: 'url' },
              { display_text: 'From env var', value: 'env' },
            ]}
            title={
              modifyTrigger.webhookUrlType === 'env' ? 'From env var' : 'URL'
            }
            dataKey={modifyTrigger.webhookUrlType === 'env' ? 'env' : 'url'}
            onButtonChange={this.handleSelectionChange}
            onInputChange={e => dispatch(setWebhookUrl(e.target.value))}
            required
            bsClass={styles.dropdown_button}
            inputVal={modifyTrigger.webhookURL}
            id="webhook-url"
            inputPlaceHolder={
              env ? 'MY_WEBHOOK_URL' : 'http://httpbin.org/post'
            }
            testId="webhook"
          />
        </div>
        {saveButton(save)}
      </div>
    );

    return (
      <div className={styles.container}>
        <div className={styles.modifySection}>
          <h4 className={styles.modifySectionHeading}>Webhook URL</h4>
          <Editor
            editorCollapsed={collapsed}
            editorExpanded={expanded}
            toggleCallback={this.setValues}
            styles={styles}
          />
        </div>
      </div>
    );
  }
}

export default WebhookEditor;
