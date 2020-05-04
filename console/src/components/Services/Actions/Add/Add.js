import React from 'react';
import styles from './Styles.scss';
import Helmet from 'react-helmet';
import HandlerEditor from '../Common/components/HandlerEditor';
import KindEditor from '../Common/components/KindEditor';
import ActionDefinitionEditor from '../Common/components/ActionDefinitionEditor';
import TypeDefinitionEditor from '../Common/components/TypeDefinitionEditor';
import HeadersConfEditor from '../Common/components/HeaderConfEditor';
import Button from '../../../Common/Button';
import {
  setActionHandler,
  setActionKind,
  setDefaults,
  setActionDefinition,
  setTypeDefinition,
  setHeaders as dispatchNewHeaders,
  toggleForwardClientHeaders as toggleFCH,
  resetDerivedActionParentOperation,
} from './reducer';
import { createAction } from '../ServerIO';
import { getActionDefinitionFromSdl } from '../../../../shared/utils/sdlUtils';

const AddAction = ({
  handler,
  dispatch,
  kind,
  actionDefinition,
  typeDefinition,
  isFetching,
  headers,
  forwardClientHeaders,
  derive,
}) => {
  React.useEffect(() => {
    if (!derive.operation) {
      dispatch(setDefaults());
    }
    return () => {
      dispatch(resetDerivedActionParentOperation());
    };
  }, []);

  const handlerOnChange = e => dispatch(setActionHandler(e.target.value));
  const kindOnChange = k => dispatch(setActionKind(k));

  const {
    sdl: typesDefinitionSdl,
    error: typesDefinitionError,
    timer: typedefParseTimer,
  } = typeDefinition;

  const {
    sdl: actionDefinitionSdl,
    error: actionDefinitionError,
    timer: actionParseTimer,
  } = actionDefinition;

  const onSubmit = () => {
    dispatch(createAction());
  };

  const setHeaders = hs => {
    dispatch(dispatchNewHeaders(hs));
  };

  const toggleForwardClientHeaders = e => {
    e.preventDefault();
    dispatch(toggleFCH());
  };

  const actionDefinitionOnChange = (value, error, timer, ast) => {
    dispatch(setActionDefinition(value, error, timer, ast));
  };

  const typeDefinitionOnChange = (value, error, timer, ast) => {
    dispatch(setTypeDefinition(value, error, timer, ast));
  };

  const allowSave =
    !isFetching &&
    !typesDefinitionError &&
    !actionDefinitionError &&
    !actionParseTimer &&
    !typedefParseTimer;

  let actionType;
  if (!actionDefinitionError) {
    // TODO optimise
    if (!actionParseTimer) {
      const { type, error } = getActionDefinitionFromSdl(actionDefinitionSdl);
      if (!error) {
        actionType = type;
      }
    }
  }

  return (
    <div>
      <Helmet title={'Add Action - Actions | Hasura'} />
      <div className={styles.heading_text}>Add a new action</div>
      <ActionDefinitionEditor
        value={actionDefinitionSdl}
        error={actionDefinitionError}
        onChange={actionDefinitionOnChange}
        timer={actionParseTimer}
        placeholder={''}
      />
      <hr />
      <TypeDefinitionEditor
        value={typesDefinitionSdl}
        error={typesDefinitionError}
        timer={typedefParseTimer}
        onChange={typeDefinitionOnChange}
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
      {actionType === 'query' ? null : (
        <React.Fragment>
          <KindEditor
            value={kind}
            onChange={kindOnChange}
            className={styles.add_mar_bottom_mid}
          />
          <hr />
        </React.Fragment>
      )}
      <HeadersConfEditor
        forwardClientHeaders={forwardClientHeaders}
        toggleForwardClientHeaders={toggleForwardClientHeaders}
        headers={headers}
        setHeaders={setHeaders}
      />
      <hr />
      <Button
        color="yellow"
        size="sm"
        type="submit"
        disabled={!allowSave}
        onClick={onSubmit}
      >
        Create
      </Button>
    </div>
  );
};

export default AddAction;
