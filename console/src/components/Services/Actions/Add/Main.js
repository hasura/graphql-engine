import React from 'react';
import styles from './Styles.scss';
import Helmet from 'react-helmet';
import NameEditor from '../Common/UIComponents/NameEditor';
import WebhookEditor from '../Common/UIComponents/WebhookEditor';
import ArgumentEditorList from '../Common/UIComponents/ArgumentEditorList';
import OutputTypesEditor from '../Common/UIComponents/OutputTypesEditor';
import Button from '../../../Common/Button';
import {
  setActionName,
  setActionWebhook,
  setActionArguments,
  setActionOutputType,
  setTypes,
} from './reducer';

const AddAction = ({
  name,
  webhook,
  arguments: args,
  outputType,
  types,
  dispatch,
}) => {
  const nameOnChange = e => dispatch(setActionName(e.target.value));
  const webhookOnChange = e => dispatch(setActionWebhook(e.target.value));
  const setArguments = a => dispatch(setActionArguments(a));
  const outputTypeOnChange = e => dispatch(setActionOutputType(e.target.value));
  const setActionTypes = t => dispatch(setTypes(t));

  const onSubmit = e => {
    e.preventDefault();
  };

  return (
    <div>
      <Helmet title={'Add Action - Actions | Hasura'} />
      <div className={styles.heading_text}>Add a new action</div>
      <form onSubmit={onSubmit}>
        <NameEditor
          value={name}
          onChange={nameOnChange}
          placeholder="action name"
          className={styles.add_mar_bottom_mid}
        />
        <hr />
        <WebhookEditor
          value={webhook}
          onChange={webhookOnChange}
          placeholder="action webhook"
          className={styles.add_mar_bottom_mid}
        />
        <hr />
        <ArgumentEditorList
          className={styles.add_mar_bottom_mid}
          args={args}
          setArguments={setArguments}
          types={types}
          setTypes={setActionTypes}
          service="create-action"
        />
        <hr />
        <OutputTypesEditor
          className={styles.add_mar_bottom_mid}
          value={outputType}
          types={types}
          setTypes={setActionTypes}
          onChange={outputTypeOnChange}
        />
        <hr />
        <Button color="yellow" size="sm">
          Create
        </Button>
      </form>
    </div>
  );
};

export default AddAction;
