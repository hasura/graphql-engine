import React from 'react';
import styles from './Styles.scss';
import Helmet from 'react-helmet';
import HandlerEditor from '../Common/UIComponents/HandlerEditor';
import KindEditor from '../Common/UIComponents/KindEditor';
import ActionDefinitionEditor from '../Common/UIComponents/ActionDefinitionEditor';
import TypeDefinitionEditor from '../Common/UIComponents/TypeDefinitionEditor';
import Button from '../../../Common/Button';
import { getModifyState } from './utils';
import {
  setModifyState,
  setActionHandler,
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
  const { handler, kind, actionDefinition, typeDefinition } = modifyProps;

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

  const handlerOnChange = e => dispatch(setActionHandler(e.target.value));
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
      <HandlerEditor
        value={handler}
        onChange={handlerOnChange}
        placeholder="action handler"
        className={styles.add_mar_bottom_mid}
        service="create-action"
      />
      <hr />
      <KindEditor value={kind} onChange={kindOnChange} />
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
