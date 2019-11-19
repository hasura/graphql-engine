import React from 'react';
import styles from './Styles.scss';
import Helmet from 'react-helmet';
import WebhookEditor from '../Common/UIComponents/WebhookEditor';
import KindEditor from '../Common/UIComponents/KindEditor';
import ActionDefinitionEditor from '../Common/UIComponents/ActionDefinitionEditor';
import TypeDefinitionEditor from '../Common/UIComponents/TypeDefinitionEditor';
import Button from '../../../Common/Button';
import { getModifyState } from './utils';
import {
  setModifyState,
  setActionWebhook,
  setActionKind,
  setActionDefinition,
  setTypeDefinition,
} from './reducer';
import { saveAction, deleteAction } from '../ServerIO';

const ActionEditor = ({
  currentAction,
  actionName,
  allTypes,
  dispatch,
  isFetching,
  ...modifyProps
}) => {
  const { webhook, kind, actionDefinition, typeDefinition } = modifyProps;

  const {
    sdl: typesDefinitionSdl,
    error: typesDefinitionError,
    timer: typeDefinitionTimer,
  } = typeDefinition;

  const {
    sdl: actionDefinitionSdl,
    error: actionDefinitionError,
    timer: actionDefinitionTimer,
  } = actionDefinition;

  // initialize action state
  const init = () => {
    const modifyState = getModifyState(currentAction, allTypes);
    dispatch(setModifyState(modifyState));
  };
  React.useEffect(init, [currentAction]);

  const webhookOnChange = e => dispatch(setActionWebhook(e.target.value));
  const kindOnChange = k => dispatch(setActionKind(k));

  const actionDefinitionOnChange = (value, error, timer, ast) => {
    dispatch(setActionDefinition(value, error, timer, ast));
  };

  const typeDefinitionOnChange = (value, error, timer, ast) => {
    dispatch(setTypeDefinition(value, error, timer, ast));
  };

  const onSave = () => {
    dispatch(saveAction(currentAction));
  };

  const onDelete = () => {
    dispatch(deleteAction(currentAction));
  };

  const allowSave =
    !isFetching &&
    !typesDefinitionError &&
    !actionDefinitionError &&
    !actionDefinitionTimer &&
    !typeDefinitionTimer;

  return (
    <div>
      <Helmet title={`Modify Action - ${actionName} Actions | Hasura`} />
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
      <ActionDefinitionEditor
        value={actionDefinitionSdl}
        error={actionDefinitionError}
        onChange={actionDefinitionOnChange}
        timer={actionDefinitionTimer}
        placeholder={''}
      />
      <hr />
      <TypeDefinitionEditor
        value={typesDefinitionSdl}
        error={typesDefinitionError}
        onChange={typeDefinitionOnChange}
        timer={typeDefinitionTimer}
        placeholder={''}
      />
      <hr />
      <div className={styles.display_flex}>
        <Button
          color="yellow"
          size="sm"
          type="submit"
          onClick={onSave}
          disabled={!allowSave}
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
