import React, { useState } from 'react';
import AceEditor from 'react-ace';

const styles = require('../Common.scss');

const NORMALKEY = 'normal';
const JSONKEY = 'json';

const parseJSONData = data => {
  try {
    if (typeof data === 'object') {
      return JSON.stringify(data, null, 4);
    }
    return JSON.stringify(JSON.parse(data), null, 4);
  } catch (e) {
    return data;
  }
};

const createInitialState = data => {
  const initialState = {
    showEditorType: NORMALKEY,
    data: parseJSONData(data),
  };
  return initialState;
};

const JsonInput = props => {
  const { standardProps, placeholderProp } = props;
  const { defaultValue, onChange } = standardProps;
  const allProps = { ...standardProps };
  delete allProps.defaultValue;
  const [state, updateState] = useState(createInitialState(defaultValue));
  const { showEditorType, data } = state;
  const updateData = (newData, currentState) => {
    return {
      ...currentState,
      data: newData,
    };
  };

  const toggleEditorType = currentState => {
    if (currentState.showEditorType === JSONKEY) {
      return {
        ...currentState,
        showEditorType: NORMALKEY,
      };
    }
    return {
      ...currentState,
      data: parseJSONData(currentState.data),
      showEditorType: JSONKEY,
    };
  };
  const handleKeyUpEvent = e => {
    if ((e.ctrlKey || event.metaKey) && e.which === 32) {
      updateState(toggleEditorType);
    }
  };

  const handleEditorExec = () => {
    updateState(toggleEditorType);
  };

  const handleInputChangeAndPropagate = e => {
    const val = e.target.value;
    updateState(currentState => updateData(val, currentState));
    if (onChange) {
      onChange(e);
    }
  };

  const handleTextAreaChangeAndPropagate = (value, e) => {
    const val = value;
    updateState(currentState => updateData(val, currentState));
    if (onChange) {
      onChange(e, value);
    }
  };

  const renderHtml =
    showEditorType === JSONKEY ? (
      <AceEditor
        key="ace_json_editor"
        {...allProps}
        mode="json"
        theme="github"
        name="jsontoggler"
        minLines={10}
        maxLines={100}
        width="100%"
        value={data}
        showPrintMargin={false}
        onChange={handleTextAreaChangeAndPropagate}
        showGutter={false}
        focus
        commands={[
          {
            name: 'toggleEditor',
            bindKey: { win: 'Ctrl-Space', mac: 'Command-Space' },
            exec: handleEditorExec,
          },
        ]}
      />
    ) : (
      <input
        key="input_json_editor"
        {...allProps}
        placeholder={`${placeholderProp} (Ctrl + Space to toggle)`}
        value={data}
        onChange={handleInputChangeAndPropagate}
        onKeyUp={handleKeyUpEvent}
      />
    );
  return (
    <span className="json_input_editor">
      <label>{renderHtml}</label>
      <i
        key="icon_json_editor"
        className={
          (state.showEditorType === JSONKEY
            ? 'fa fa-compress '
            : 'fa fa-expand ') + styles.jsonButtonAlign
        }
        onClick={() => updateState(toggleEditorType)}
      />
    </span>
  );
};
export default JsonInput;
