import React from 'react';
import CustomTypesContainer from '../../Containers/CustomTypesContainer';
import TypesEditor from '../../Common/components/TypeDefinitionEditor';
import Button from '../../../../Common/Button/Button';
import styles from '../../Common/components/Styles.scss';
import {
  getTypesSdl,
  getTypesFromSdl,
} from '../../../../../shared/utils/sdlUtils';
import { setTypeDefinition, setFetching, unsetFetching } from '../reducer';
import { setCustomGraphQLTypes } from '../../../Types/ServerIO';
import { showErrorNotification } from '../../../Common/Notification';

const Manage = ({ allTypes, dispatch, ...manageProps }) => {
  const sdlOnChange = (value, _error, _timer, ast) => {
    dispatch(setTypeDefinition(value, _error, _timer, ast));
  };

  const init = () => {
    const existingTypeDefSdl = getTypesSdl(allTypes);
    sdlOnChange(existingTypeDefSdl);
  };

  React.useEffect(init, []);

  const {
    manage: {
      definition: { sdl, error, timer },
    },
    isFetching,
  } = manageProps;

  const onSave = () => {
    const { types: newTypes, error: _error } = getTypesFromSdl(sdl);
    if (_error) {
      dispatch(showErrorNotification('Invalid Types Definition', _error));
    }
    const stopFetching = () => dispatch(unsetFetching());
    dispatch(setFetching());
    dispatch(
      setCustomGraphQLTypes(newTypes, stopFetching, stopFetching, stopFetching)
    );
  };

  // TODO handling error elegantly
  const allowSave = !isFetching && !error;

  const editorTooltip = 'All GraphQL types used in actions';
  const editorLabel = 'All custom types';

  return (
    <CustomTypesContainer tabName="manage" dispatch={dispatch}>
      <TypesEditor
        value={sdl}
        error={error}
        timer={timer}
        onChange={sdlOnChange}
        placeholder={''}
        label={editorLabel}
        tooltip={editorTooltip}
        editorHeight="600px"
      />
      <hr />
      <Button
        onClick={onSave}
        disabled={!allowSave}
        color="yellow"
        className={styles.add_mar_right}
      >
        Save
      </Button>
      <Button onClick={init} color="white">
        Reset
      </Button>
    </CustomTypesContainer>
  );
};

export default Manage;
