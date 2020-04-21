import React from 'react';
import { parse as sdlParse } from 'graphql/language/parser';

import SDLEditor from '../../../../Common/AceEditor/SDLEditor';
import { Icon, ToolTip } from '../../../../UIKit/atoms';
import styles from './Styles.scss';

const editorLabel = 'Action definition';
const editorTooltip =
  'Define the action as a query or a mutation using GraphQL SDL. You can use the custom types already defined by you or define new types in the new types definition editor below.';

const ActionDefinitionEditor = ({
  value,
  onChange,
  className,
  placeholder,
  error,
  timer,
}) => {
  const onChangeWithError = v => {
    if (timer) {
      clearTimeout(timer);
    }

    const parseDebounceTimer = setTimeout(() => {
      let _e = null;
      let ast = null;
      try {
        ast = sdlParse(v);
      } catch (e) {
        _e = e;
      }
      onChange(null, _e, null, ast);
    }, 1000);

    onChange(v, null, parseDebounceTimer, null);
  };

  const errorMessage =
    error && (error.message || 'This is not valid GraphQL SDL');

  let markers = [];
  if (error && error.locations) {
    markers = error.locations.map(l => ({
      row: l.line,
      column: l.column,
      type: 'error',
      message: errorMessage,
      className: styles.errorMarker,
    }));
  }

  return (
    <div className={`${className || ''}`}>
      <h2
        className={`${styles.subheading_text} ${styles.add_mar_bottom_small}`}
      >
        {editorLabel}
        <ToolTip message={editorTooltip} ml="sm" />
      </h2>
      <div className={styles.sdlEditorContainer}>
        <div
          className={`${styles.display_flex} ${styles.add_mar_bottom_small}`}
        >
          {error && (
            <div className={`${styles.display_flex}  ${styles.errorMessage}`}>
              <Icon type="close" color="red.primary" mr="xs" mb="12px" />
              <div>{errorMessage}</div>
            </div>
          )}
        </div>
        <SDLEditor
          name="sdl-editor"
          value={value}
          onChange={onChangeWithError}
          placeholder={placeholder}
          markers={markers}
          height="200px"
          width="600px"
        />
      </div>
    </div>
  );
};

export default ActionDefinitionEditor;
