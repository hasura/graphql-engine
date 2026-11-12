import React from 'react';
import { parse as sdlParser } from 'graphql/language/parser';
import { DocumentNode, GraphQLError } from 'graphql';
import { Analytics, REDACT_EVERYTHING } from '@hasura/shared/analytics';
import { Flex, Heading } from '@radix-ui/themes';
import { IconTooltip, AceEditor } from '@hasura/shared/ui';

type GraphQLEditorProps = {
  value: string;
  onChange: (
    value: string | null,
    error: GraphQLError | null,
    timer: NodeJS.Timeout | null,
    ast: Record<string, any> | null,
  ) => void;
  className?: string;
  fontSize?: string;
  error?: GraphQLError | null;
  timer?: NodeJS.Timeout | null;
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
      let timerError: GraphQLError | null = null;
      let ast: DocumentNode | null = null;
      try {
        ast = sdlParser(val);
      } catch (err) {
        timerError = err as GraphQLError;
      }

      onChange(val, timerError, null, ast);
    }, 1000);

    onChange(val, null, parseDebounceTimer, null);
  };

  // the parse error is shown in the editor's gutter, on its line
  const errorLocation = error?.locations?.[0];
  const annotations = error
    ? [
        {
          row: errorLocation ? errorLocation.line - 1 : 0,
          column: errorLocation ? errorLocation.column - 1 : 0,
          type: 'error',
          text: error.message || 'This is not valid GraphQL SDL',
        },
      ]
    : [];

  return (
    <Analytics name="GraphiQLEditor" {...REDACT_EVERYTHING}>
      <div className={className || 'w-full'}>
        {label ? (
          <Flex align="center" gap="2">
            <Heading size="3">{label}</Heading>
            {tooltip ? <IconTooltip message={tooltip} /> : null}
          </Flex>
        ) : null}
        <div className="my-2 relative">
          <AceEditor
            name="sdl-editor"
            value={value}
            fontSize={fontSize}
            onChange={onChangeWithError}
            placeholder={placeholder}
            height={height || '200px'}
            mode="graphqlschema"
            width={width || '100%'}
            showPrintMargin={false}
            disabled={readOnlyMode}
            annotations={annotations}
            setOptions={{
              useWorker: false,
              showLineNumbers: true,
            }}
          />
        </div>
      </div>
    </Analytics>
  );
};

export default GraphQLEditor;
