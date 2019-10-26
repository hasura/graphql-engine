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
import { getModifyState } from './utils';
import {
  setModifyState,
  setActionName,
  setActionWebhook,
  setActionKind,
  setActionOutputType,
  setActionArguments,
  setTypes,
  setTypesBulk,
} from './reducer';
import { saveAction, deleteAction } from '../ServerIO';
import { defaultArg, defaultScalarType } from '../Common/stateDefaults';

const ActionEditor = ({
  currentAction,
  actionName,
  allTypes,
  dispatch,
  ...modifyProps
}) => {
  const {
    name,
    webhook,
    arguments: args,
    types,
    outputType,
    kind,
    isFetching,
  } = modifyProps;

  // initialize action state
  const init = () => {
    const modifyState = getModifyState(currentAction, allTypes);
    dispatch(setModifyState(modifyState));
  };
  React.useEffect(init, [currentAction]);

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

  const onSave = () => {
    dispatch(saveAction(currentAction));
  };
  const onDelete = () => {
    dispatch(deleteAction(currentAction));
  };

  return (
    <div>
      <Helmet title={`Modify Action - ${actionName} Actions | Hasura`} />
      <div className={styles.heading_text}>Modify action</div>
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
      <div className={styles.display_flex}>
        <Button
          color="yellow"
          size="sm"
          type="submit"
          onClick={onSave}
          disabled={isFetching}
          className={styles.add_mar_right}
        >
          Save
        </Button>
        <Button
          color="red"
          size="sm"
          type="submit"
          onClick={onDelete}
          disabled={isFetching}
        >
          Delete
        </Button>
      </div>
    </div>
  );
};

export default ActionEditor;
