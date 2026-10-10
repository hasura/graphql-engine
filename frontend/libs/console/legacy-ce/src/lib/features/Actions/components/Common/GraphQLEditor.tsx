import React from 'react';
import { parse as sdlParser } from 'graphql/language/parser';
import { DocumentNode, GraphQLError } from 'graphql';
import { Analytics, REDACT_EVERYTHING } from '@hasura/shared/analytics';
import { Flex, Heading } from '@radix-ui/themes';
import { IconTooltip, AceEditor, IndicatorCard } from '@hasura/shared/ui';

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

  const errorMessage =
    error && (error.message || 'This is not valid GraphQL SDL');

  const errorMessageLine =
    error &&
    error.locations &&
    error.locations.length &&
    ` at line ${error.locations[0].line}, column ${error.locations[0].column} `;

  return (
    <Analytics name="GraphiQLEditor" {...REDACT_EVERYTHING}>
      <div className={className || 'w-full'}>
        {label ? (
          <Flex align="center" gap="2">
            <Heading size="3">{label}</Heading>
            {tooltip ? <IconTooltip message={tooltip} /> : <></>}
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
            setOptions={{
              useWorker: false,
              showLineNumbers: true,
            }}
          />
          {error && (
            <div className="absolute bottom-0 left-[48px] w-[calc(100%-48px)]">
              <IndicatorCard className="py-1!" status="negative" size="1">
                {errorMessage} {errorMessageLine}
              </IndicatorCard>
            </div>
          )}
        </div>
      </div>
    </Analytics>
  );
};

export default GraphQLEditor;
