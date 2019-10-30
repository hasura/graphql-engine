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

    // purge arguments of the removed type
    newArgs = newArgs
      .map(a => {
        const argTypeIndex = parseInt(a.type, 10);
        if (argTypeIndex != index) {
          return {
            ...a,
            type:
              argTypeIndex > index
                ? (argTypeIndex - 1).toString()
                : argTypeIndex.toString(),
          };
        }
        return null;
      })
      .filter(a => !!a);

    // purge type fields of the removed type
    newTypes = newTypes
      .map((t, i) => {
        if (i == index) return null;
        if (t.kind === 'scalar' || t.kind === 'enum' || t.isInbuilt) return t;
        const _t = { ...t };
        _t.fields = _t.fields
          .map(f => {
            if (f.type != index) {
              const typeIndex = parseInt(f.type, 10);
              return {
                ...f,
                type:
                  typeIndex > index
                    ? (typeIndex - 1).toString()
                    : typeIndex.toString(),
              };
            }
            return null;
          })
          .filter(f => !!f);
        return _t;
      })
      .filter(t => !!t);

    // purge output type of the removed index
    let newOutputType = outputType;
    if (outputType) {
      const outputTypeIndex = parseInt(outputType, 10);
      if (outputTypeIndex == index) {
        newOutputType = '';
      } else {
        newOutputType =
          outputTypeIndex > index
            ? (outputTypeIndex - 1).toString()
            : outputTypeIndex.toString();
      }
    }

    dispatch(setTypesBulk([...newTypes], newArgs, newOutputType));
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
