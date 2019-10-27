import React from 'react';
import styles from './Styles.scss';
import Helmet from 'react-helmet';
import NameEditor from '../Common/UIComponents/NameEditor';
import WebhookEditor from '../Common/UIComponents/WebhookEditor';
import KindEditor from '../Common/UIComponents/KindEditor';
import ArgumentEditorList from '../Common/UIComponents/ArgumentEditorList';
import OutputTypesEditor from '../Common/UIComponents/OutputTypesEditor';
import TypeEditorList from '../Common/UIComponents/TypeEditorList';
import Button from '../../../Common/Button';
import {
  setActionName,
  setActionWebhook,
  setActionKind,
  setActionArguments,
  setActionOutputType,
  setTypes,
  setTypesBulk,
  setDefaults,
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
  kind,
  isFetching,
}) => {
  React.useEffect(() => {
    dispatch(setDefaults());
  }, []);

  const nameOnChange = e => dispatch(setActionName(e.target.value));
  const webhookOnChange = e => dispatch(setActionWebhook(e.target.value));
  const kindOnChange = k => dispatch(setActionKind(k));
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
      if (t.kind === 'scalar' || t.kind === 'enum' || t.isInbuilt) return t;
      const _t = { ...t };
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
      <KindEditor value={kind} onChange={kindOnChange} />
      <hr />
      <TypeEditorList
        types={types}
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
        allTypes={types}
        service="create-action"
      />
      <hr />
      <OutputTypesEditor
        className={styles.add_mar_bottom_mid}
        value={outputType}
        allTypes={types}
        onChange={outputTypeOnChange}
        service="create-action"
      />
      <hr />
      <Button
        color="yellow"
        size="sm"
        type="submit"
        disabled={isFetching}
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
