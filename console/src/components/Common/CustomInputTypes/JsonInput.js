import React, { useState } from 'react';
import AceEditor from 'react-ace';

const styles = require('./JsonInput.scss');

const NORMALKEY = 'normal';
const JSONKEY = 'json';

const parseJSONData = (data, editorType) => {
  try {
    const dataObject = typeof data === 'object' ? data : JSON.parse(data);

    return JSON.stringify(dataObject, null, editorType === JSONKEY ? 4 : 0);
  } catch (e) {
    return data;
  }
};

const createInitialState = data => {
  const initialState = {
    editorType: NORMALKEY,
    data: parseJSONData(data, NORMALKEY),
  };
  return initialState;
};

const JsonInput = props => {
  const { standardProps, placeholderProp } = props;
  const { defaultValue, onChange } = standardProps;
  const allProps = { ...standardProps };
  delete allProps.defaultValue;
  const [state, updateState] = useState(createInitialState(defaultValue));
  const { editorType, data } = state;

  const updateData = (newData, currentState) => {
    return {
      ...currentState,
      data: newData,
    };
  };

  const toggleEditorType = currentState => {
    const nextEditorType =
      currentState.editorType === JSONKEY ? NORMALKEY : JSONKEY;

    return {
      ...currentState,
      data: parseJSONData(currentState.data, nextEditorType),
      editorType: nextEditorType,
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

  const getJsonEditor = () => {
    return (
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
    );
  };

  const getNormalEditor = () => {
    return (
      <input
        key="input_json_editor"
        {...allProps}
        placeholder={placeholderProp}
        value={data}
        onChange={handleInputChangeAndPropagate}
        onKeyUp={handleKeyUpEvent}
        className={allProps.className + ' ' + styles.jsonNormalInput}
      />
    );
  };

  const editor = editorType === JSONKEY ? getJsonEditor() : getNormalEditor();

  return (
    <span className="json_input_editor">
      <label>{editor}</label>
      <i
        key="icon_json_editor"
        className={
          'fa ' +
          styles.jsonToggleButton +
          (editorType === JSONKEY ? ' fa-compress' : ' fa-expand')
        }
        onClick={() => updateState(toggleEditorType)}
        title={
          (editorType === JSONKEY ? 'Collapse' : 'Expand') + '(Ctrl + Space)'
        }
      />
    </span>
  );
};
export default JsonInput;
