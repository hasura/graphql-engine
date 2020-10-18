import React from 'react';
import { parse as sdlParser } from 'graphql/language/parser';
import { GraphQLError } from 'graphql';
import styles from './GraphQlEditor.scss';
import CrossIcon from '../Icons/Cross';
import AceEditor from '../AceEditor/BaseEditor';
import { Nullable } from '../utils/tsUtils';

type GraphQLEditorProps = {
  value: string;
  onChange: (
    value: Nullable<string>,
    error: Nullable<Error>,
    timer: Nullable<NodeJS.Timeout>,
    ast: Nullable<Record<string, any>>
  ) => void;
  placeholder: string;
  error: GraphQLError;
  timer: number;
  readOnlyMode: boolean;
  height?: string;
  allowEmpty?: boolean;
};

const GraphQLEditor: React.FC<GraphQLEditorProps> = ({
  value,
  onChange,
  placeholder = '',
  error,
  timer,
  readOnlyMode,
  height,
  allowEmpty = false,
}) => {
  const onChangeWithError = (val: string) => {
    if (timer) {
      clearTimeout(timer);
    }

    const parseDebounceTimer = setTimeout(() => {
      if (allowEmpty && val === '') {
        return onChange(val, null, null, null);
      }
      let timerError = null;
      let ast = null;
      try {
        ast = sdlParser(val);
      } catch (err) {
        timerError = err;
      }
      onChange(null, timerError, null, ast);
    }, 1000);

    onChange(val, null, parseDebounceTimer, null);
  };

  const errorMessage =
    error && (error.message || 'This is not valid GraphQL SDL');

  const errorMessageLine =
    error &&
    error.locations &&
    error.locations.length &&
    ` at line ${error.locations[0].line}, column ${error.locations[0].column} `;

  return (
    <>
      <div className={`${styles.display_flex} ${styles.add_mar_bottom_small}`}>
        {error && (
          <div className={`${styles.display_flex}  ${styles.errorMessage}`}>
            <CrossIcon className={styles.add_mar_right_small} />
            <div>{`${errorMessage} ${errorMessageLine}`}</div>
          </div>
        )}
      </div>
      <AceEditor
        name="sdl-editor"
        value={value}
        onChange={onChangeWithError}
        placeholder={placeholder}
        height={height || '200px'}
        mode="graphqlschema"
        width="600px"
        readOnly={readOnlyMode}
      />
    </>
  );
};

export default GraphQLEditor;
