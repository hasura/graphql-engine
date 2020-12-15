import React from 'react';
import styles from './Styles.scss';
import Helmet from 'react-helmet';
import HandlerEditor from '../Common/components/HandlerEditor';
import KindEditor from '../Common/components/KindEditor';
import HeaderConfEditor from '../Common/components/HeaderConfEditor';
import Button from '../../../Common/Button';
import { getModifyState } from './utils';
import {
  setModifyState,
  setActionHandler,
  setActionKind,
  setActionDefinition,
  setTypeDefinition,
  setHeaders as dispatchNewHeaders,
  toggleForwardClientHeaders as toggleFCH,
} from './reducer';
import { saveAction, deleteAction } from '../ServerIO';
import { getActionDefinitionFromSdl } from '../../../../shared/utils/sdlUtils';
import GraphQLEditor from '../Common/components/GraphQLEditor';
import { typeDefinitionInfo } from '../Add/Add';

export const actionDefinitionInfo = {
  label: 'Action definition',
  tooltip:
    'Define the action as a query or a mutation using GraphQL SDL. You can use the custom types already defined by you or define new types in the new types definition editor below.',
};

const ActionEditor = ({
  currentAction,
  actionName,
  allTypes,
  dispatch,
  isFetching,
  headers,
  forwardClientHeaders,
  readOnlyMode,
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
  React.useEffect(init, [currentAction, allTypes, dispatch]);

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

  const setHeaders = hs => {
    dispatch(dispatchNewHeaders(hs));
  };

  const toggleForwardClientHeaders = () => {
    dispatch(toggleFCH());
  };

  const allowSave =
    !isFetching &&
    !typesDefinitionError &&
    !actionDefinitionError &&
    !actionDefinitionTimer &&
    !typeDefinitionTimer;

  let actionType;
  if (!actionDefinitionError) {
    // TODO optimise
    const { type, error } = getActionDefinitionFromSdl(actionDefinitionSdl);
    if (!error) {
      actionType = type;
    }
  }

  return (
    <div>
      <Helmet title={`Modify Action - ${actionName} - Actions | Hasura`} />
      <GraphQLEditor
        value={actionDefinitionSdl}
        error={actionDefinitionError}
        onChange={actionDefinitionOnChange}
        timer={actionDefinitionTimer}
        readOnlyMode={readOnlyMode}
        label={actionDefinitionInfo.label}
        tooltip={actionDefinitionInfo.tooltip}
      />
      <hr />
      <GraphQLEditor
        value={typesDefinitionSdl}
        error={typesDefinitionError}
        onChange={typeDefinitionOnChange}
        timer={typeDefinitionTimer}
        readOnlyMode={readOnlyMode}
        label={typeDefinitionInfo.label}
        tooltip={typeDefinitionInfo.tooltip}
        allowEmpty
      />
      <hr />
      <HandlerEditor
        value={handler}
        disabled={readOnlyMode}
        onChange={handlerOnChange}
        placeholder="action handler"
        className={styles.add_mar_bottom_mid}
        service="create-action"
      />
      <hr />
      {actionType === 'query' ? null : (
        <React.Fragment>
          <KindEditor
            value={kind}
            onChange={kindOnChange}
            className={styles.add_mar_bottom_mid}
            disabled={readOnlyMode}
          />
          <hr />
        </React.Fragment>
      )}
      <HeaderConfEditor
        forwardClientHeaders={forwardClientHeaders}
        toggleForwardClientHeaders={toggleForwardClientHeaders}
        headers={headers}
        setHeaders={setHeaders}
        disabled={readOnlyMode}
      />
      <hr />
      <div className={styles.display_flex}>
        {!readOnlyMode && (
          <React.Fragment>
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
          </React.Fragment>
        )}
      </div>
    </div>
  );
};

export default ActionEditor;
