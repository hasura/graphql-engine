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
  const updateData = newData => {
    return {
      ...state,
      data: newData,
    };
  };
  const toggleEditorType = () => {
    if (state.showEditorType === JSONKEY) {
      return {
        ...state,
        showEditorType: NORMALKEY,
      };
    }
    return {
      ...state,
      data: parseJSONData(state.data),
      showEditorType: JSONKEY,
    };
  };
  const handleKeyUpEvent = e => {
    if ((e.ctrlKey || event.metaKey) && e.which === 32) {
      updateState(toggleEditorType());
    }
  };

  const handleInputChangeAndPropagate = e => {
    updateState(updateData(e.target.value));
    if (onChange) {
      onChange(e);
    }
  };

  const handleTextAreaChangeAndPropagate = (value, e) => {
    updateState(updateData(value));
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
            exec: () => updateState(toggleEditorType()),
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
        onClick={() => updateState(toggleEditorType())}
      />
    </span>
  );
};
export default JsonInput;
