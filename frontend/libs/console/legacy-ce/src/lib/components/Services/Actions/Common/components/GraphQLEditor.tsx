import React from 'react';
import { parse as sdlParser } from 'graphql/language/parser';
import { GraphQLError } from 'graphql';
import {
  Analytics,
  REDACT_EVERYTHING,
} from '../../../../../features/Analytics';
import { IconTooltip } from '../../../../../new-components/Tooltip';
import CrossIcon from '../../../../Common/Icons/Cross';
import AceEditor from '../../../../Common/AceEditor/BaseEditor';
import { Nullable } from '../../../../Common/utils/tsUtils';

type GraphQLEditorProps = {
  value: string;
  onChange: (
    value: Nullable<string>,
    error: Nullable<GraphQLError>,
    timer: Nullable<NodeJS.Timeout>,
    ast: Nullable<Record<string, any>>
  ) => void;
  className?: string;
  fontSize?: string;
  error: Nullable<GraphQLError>;
  timer: Nullable<NodeJS.Timeout>;
  readOnlyMode: boolean;
  placeholder?: string;
  label?: string | undefined;
  tooltip?: string | undefined;
  height?: string;
  width?: string;
  allowEmpty?: boolean;
};

const GraphQLEditor: React.FC<GraphQLEditorProps> = ({
  value,
  onChange,
  className,
  fontSize,
  error,
  timer,
  readOnlyMode,
  placeholder = '',
  label,
  tooltip,
  height,
  width,
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
      onChange(null, timerError as GraphQLError, null, ast);
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
    <Analytics name="GraphiQLEditor" {...REDACT_EVERYTHING}>
      <div className={`${className || ''}`}>
        {label ? (
          <h2 className="text-lg font-bold pb-5 mb-1.5">
            {label}
            {tooltip ? <IconTooltip message={tooltip} /> : <></>}
          </h2>
        ) : null}
        <div className="flex mb-1.5">
          {error && (
            <div className="flex text-red-600">
              <CrossIcon className="mr-1.5" />
              <div>{`${errorMessage} ${errorMessageLine}`}</div>
            </div>
          )}
        </div>
        <AceEditor
          name="sdl-editor"
          value={value}
          fontSize={fontSize}
          onChange={onChangeWithError}
          placeholder={placeholder}
          height={height || '200px'}
          mode="graphqlschema"
          width={width || '600px'}
          showPrintMargin={false}
          readOnly={readOnlyMode}
          setOptions={{ useWorker: false }}
        />
      </div>
    </Analytics>
  );
};

export default GraphQLEditor;
