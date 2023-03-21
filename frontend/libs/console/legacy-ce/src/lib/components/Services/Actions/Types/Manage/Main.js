import React from 'react';
import CustomTypesContainer from '../../Containers/CustomTypesContainer';
import { Button } from '../../../../../new-components/Button';
import {
  getTypesSdl,
  getTypesFromSdl,
} from '../../../../../shared/utils/sdlUtils';
import { setTypeDefinition, setFetching, unsetFetching } from '../reducer';
import { setCustomGraphQLTypes } from '../../../Types/ServerIO';
import { showErrorNotification } from '../../../Common/Notification';
import ToolTip from '../../../../Common/Tooltip/Tooltip';
import GraphQLEditor from '../../Common/components/GraphQLEditor';

const Manage = ({ allTypes, dispatch, readOnlyMode, ...manageProps }) => {
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
  const allowSave = !isFetching && !error && !readOnlyMode;

  const editorTooltip = 'All GraphQL types used in actions';
  const editorLabel = 'All custom types';

  return (
    <div className="bootstrap-jail">
      <CustomTypesContainer tabName="manage" dispatch={dispatch}>
        <GraphQLEditor
          value={sdl}
          error={error}
          timer={timer}
          onChange={sdlOnChange}
          placeholder={''}
          label={editorLabel}
          tooltip={editorTooltip}
          height="600px"
          readOnlyMode={readOnlyMode}
          fontSize="14px"
          allowEmpty
        />
        <hr className="my-md" />
        <div className="flex">
          <div className="mr-5">
            <Button onClick={onSave} disabled={!allowSave} mode="primary">
              Save
            </Button>
          </div>
          <Button onClick={init} mode="default" disabled={readOnlyMode}>
            Reset
          </Button>
        </div>
        {readOnlyMode && (
          <ToolTip message="Modifying custom type is not allowed in Read only mode!" />
        )}
      </CustomTypesContainer>
    </div>
  );
};

export default Manage;
