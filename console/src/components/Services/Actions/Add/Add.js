import React from 'react';
import styles from './Styles.scss';
import Helmet from 'react-helmet';
import WebhookEditor from '../Common/UIComponents/WebhookEditor';
import KindEditor from '../Common/UIComponents/KindEditor';
import ActionDefinitionEditor from '../Common/UIComponents/ActionDefinitionEditor';
import TypeDefinitionEditor from '../Common/UIComponents/TypeDefinitionEditor';
import Button from '../../../Common/Button';
import {
  setActionWebhook,
  setActionKind,
  setDefaults,
  setActionDefinition,
  setTypeDefinition,
} from './reducer';
import { createAction } from '../ServerIO';

const AddAction = ({
  webhook,
  dispatch,
  kind,
  actionDefinition,
  typeDefinition,
  isFetching,
}) => {
  React.useEffect(() => {
    dispatch(setDefaults());
  }, []);

  const webhookOnChange = e => dispatch(setActionWebhook(e.target.value));
  const kindOnChange = k => dispatch(setActionKind(k));

  const {
    sdl: typesDefinitionSdl,
    error: typesDefinitionError,
  } = typeDefinition;

  const {
    sdl: actionDefinitionSdl,
    error: actionDefinitionError,
  } = actionDefinition;

  const onSubmit = () => {
    dispatch(createAction());
  };

  const actionDefinitionOnChange = (value, error) => {
    dispatch(setActionDefinition(value, error));
  };

  const typeDefinitionOnChange = (value, error) => {
    dispatch(setTypeDefinition(value, error));
  };

  const allowSave =
    !isFetching && !typesDefinitionError && !actionDefinitionError;

  return (
    <div>
      <Helmet title={'Add Action - Actions | Hasura'} />
      <div className={styles.heading_text}>Add a new action</div>
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
        placeholder={''}
      />
      <hr />
      <TypeDefinitionEditor
        value={typesDefinitionSdl}
        error={typesDefinitionError}
        onChange={typeDefinitionOnChange}
        placeholder={''}
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
