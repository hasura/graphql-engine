import React, { useState } from 'react';
import AceEditor from 'react-ace';

import 'brace/mode/html';
import 'brace/mode/markdown';
import 'brace/theme/github';
import 'brace/theme/chrome';

const styles = require('./CustomInput.scss');

// editorType is what sort of editor. All are ACE Editor
// modes except 0, which is text input

// ACE editor  mode names
const EDITORTYPES = [
  'normal', // must be first
  'text',
  'markdown',
  'html',
];

// human readable editor names
// const EDITORTYPENAMES = [
//   'single line input',
//   'multi-line text input',
//   'markdown',
//   'html',
// ];

// short human readable editor names
// for the visible label
// const SHORT_EDITOR_TYPE_NAMES = ['', 'multi-line', 'markdown', 'html'];

const NORMAL_KEY = 0;
const MULTILINE_KEY = 1;

const createInitialState = data => {
  const initialState = {
    editorType: NORMAL_KEY,
    data: data,
  };
  return initialState;
};

const TextInput = props => {
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

  const cycleEditorType = currentState => {
    // const nextEditorType = (currentState.editorType + 1) % EDITORTYPES.length;
    const nextEditorType =
      currentState.editorType === NORMAL_KEY ? MULTILINE_KEY : NORMAL_KEY;

    return {
      ...currentState,
      editorType: nextEditorType,
    };
  };

  const handleKeyUpEvent = e => {
    if ((e.ctrlKey || event.metaKey) && e.which === 32) {
      updateState(cycleEditorType);
    }
  };

  const handleEditorExec = () => {
    updateState(cycleEditorType);
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

  const getAceEditor = curmode => {
    return (
      <AceEditor
        key="ace_text_editor"
        {...allProps}
        mode={curmode}
        theme="chrome"
        name="texttoggler"
        minLines={10}
        maxLines={30}
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
        key="input_text_editor"
        {...allProps}
        placeholder={placeholderProp}
        value={data}
        onChange={handleInputChangeAndPropagate}
        onKeyUp={handleKeyUpEvent}
        className={allProps.className + ' ' + styles.normalInput}
      />
    );
  };

  const editor =
    editorType === NORMAL_KEY
      ? getNormalEditor()
      : getAceEditor(EDITORTYPES[editorType]);

  // const cycleIcon = (
  //   <span
  //     onClick={() => updateState(cycleEditorType)}
  //     title={
  //       'Change to ' +
  //       EDITORTYPENAMES[(editorType + 1) % EDITORTYPES.length] +
  //       ' (Ctrl + Space)'
  //     }
  //   >
  //     <span className={styles.modeType}>
  //       {SHORT_EDITOR_TYPE_NAMES[editorType]}
  //     </span>
  //     <i
  //       key="icon_text_editor"
  //       className={
  //         'fa ' +
  //         styles.modeToggleButton +
  //         (editorType === NORMAL_KEY ? ' fa-expand' : ' fa-chevron-right')
  //       }
  //     />
  //   </span>
  // );

  const cycleIcon = (
    <i
      key="icon_text_editor"
      className={
        'fa ' +
        styles.modeToggleButton +
        (editorType === MULTILINE_KEY ? ' fa-compress' : ' fa-expand')
      }
      onClick={() => updateState(cycleEditorType)}
      title={
        (editorType === MULTILINE_KEY ? 'Collapse' : 'Expand') +
        ' (Ctrl + Space)'
      }
    />
  );

  return (
    <span className="text_input_editor">
      <label>{editor}</label>
      {cycleIcon}
    </span>
  );
};
export default TextInput;
