import React from 'react';
import styles from './Styles.scss';
import Helmet from 'react-helmet';
import NameEditor from '../Common/UIComponents/NameEditor';
import WebhookEditor from '../Common/UIComponents/WebhookEditor';
import ArgumentEditorList from '../Common/UIComponents/ArgumentEditorList';
import OutputTypesEditor from '../Common/UIComponents/OutputTypesEditor';
import TypeEditorList from '../Common/UIComponents/TypeEditorList';
import Button from '../../../Common/Button';
import {
  setActionName,
  setActionWebhook,
  setActionArguments,
  setActionOutputType,
  setTypes,
} from './reducer';
import { defaultScalars } from '../Common/utils';
import { createAction } from '../ServerIO';

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
  const outputTypeOnChange = e => dispatch(setActionOutputType(e.target.value));
  const setArguments = a => {
    const newArgs = [...a];
    const lastArg = newArgs[newArgs.length - 1];
    if (lastArg.name && lastArg.type) {
      newArgs.push({ name: '', type: '', description: '', optional: false });
    }
    dispatch(setActionArguments(newArgs));
  };
  const setActionTypes = t => {
    const newTypes = [...t];
    const lastType = newTypes[newTypes.length - 1];
    if (lastType.name && lastType.kind) {
      newTypes.push({ name: '', kind: '' });
    }
    dispatch(setTypes(newTypes));
  };

  const onSubmit = e => {
    if (e) {
      e.preventDefault();
    }
    dispatch(createAction());
  };

  const argTypes = [
    ...defaultScalars,
    ...types.filter(t => !!t.name && t.kind !== 'object').map(t => t.name),
  ].sort();

  const fieldTypes = [
    ...defaultScalars,
    ...types
      .filter(t => !!t.name && t.kind !== 'input_object')
      .map(t => t.name),
  ].sort();

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
          service="create-action"
        />
        <hr />
        <WebhookEditor
          value={webhook}
          onChange={webhookOnChange}
          placeholder="action webhook"
          className={styles.add_mar_bottom_mid}
          service="create-action"
        />
        <hr />
        <TypeEditorList
          types={types}
          argTypes={argTypes}
          fieldTypes={fieldTypes}
          setTypes={setActionTypes}
          className={styles.add_mar_bottom_mid}
          service="create-action"
        />
        <hr />
        <ArgumentEditorList
          className={styles.add_mar_bottom_mid}
          args={args}
          setArguments={setArguments}
          allTypes={argTypes}
          service="create-action"
        />
        <hr />
        <OutputTypesEditor
          className={styles.add_mar_bottom_mid}
          value={outputType}
          allTypes={fieldTypes}
          onChange={outputTypeOnChange}
          service="create-action"
        />
        <hr />
        <Button
          color="yellow"
          size="sm"
          type="submit"
          onClick={() => {
            console.log('here');
            onSubmit();
          }}
        >
          Create
        </Button>
      </form>
    </div>
  );
};

export default AddAction;
