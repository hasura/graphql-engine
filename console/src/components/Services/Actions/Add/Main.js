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
  setTypesBulk,
} from './reducer';
import { createAction } from '../ServerIO';
import { defaultArg, defaultScalarType } from '../Common/stateDefaults';

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
      newArgs.push(defaultArg);
    }
    dispatch(setActionArguments(newArgs));
  };

  const setActionTypes = t => {
    const newTypes = [...t];
    const lastType = newTypes[newTypes.length - 1];
    if (lastType.name && lastType.kind) {
      newTypes.push(defaultScalarType);
    }
    dispatch(setTypes(newTypes));
  };

  const removeType = index => {
    let newArgs = JSON.parse(JSON.stringify(args));
    let newTypes = JSON.parse(JSON.stringify(types));
    newArgs = newArgs.filter(a => a.type != index);
    newTypes = newTypes.map(t => {
      if (t.kind === 'scalar' || t.isInbuilt) return t;
      const _t = { ...t };
      if (t.kind === 'object') {
        _t.arguments = _t.arguments.filter(a => a.type != index);
      }
      _t.fields = _t.fields.filter(f => f.type != index);
      return _t;
    });
    dispatch(
      setTypesBulk(
        [...newTypes.slice(0, index), ...newTypes.slice(index + 1)],
        newArgs,
        outputType == index ? '' : outputType
      )
    );
  };

  const onSubmit = e => {
    if (e) {
      e.preventDefault();
    }
    dispatch(createAction());
  };

  const argTypes = [...types.filter(t => !!t.name && t.kind !== 'object')];

  const objectFieldTypes = [
    ...types.filter(t => !!t.name && t.kind !== 'input_object'),
  ];

  return (
    <div>
      <Helmet title={'Add Action - Actions | Hasura'} />
      <div className={styles.heading_text}>Add a new action</div>
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
        fieldTypes={objectFieldTypes}
        setTypes={setActionTypes}
        removeType={removeType}
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
        allTypes={objectFieldTypes}
        onChange={outputTypeOnChange}
        service="create-action"
      />
      <hr />
      <Button
        color="yellow"
        size="sm"
        type="submit"
        onClick={() => {
          onSubmit();
        }}
      >
        Create
      </Button>
    </div>
  );
};

export default AddAction;
