import React from 'react';
import clsx from 'clsx';
import get from 'lodash.get';
import AceEditor, {
  IAceOptions,
  IAceEditorProps,
  ICommandManager,
} from 'react-ace';
import 'ace-builds/src-noconflict/theme-github';
import 'ace-builds/src-noconflict/theme-eclipse';
import 'ace-builds/src-noconflict/ext-language_tools';
import { FieldError, useFormContext, Controller } from 'react-hook-form';
import { FieldWrapper, FieldWrapperPassThroughProps } from './FieldWrapper';

// Allows to integrate the code editor in a form: by default, tab key adds a
// tab character to the editor content, here, we want to disable this behavior
// and allow to navigate with the tab key. This is done by setting the command.
// Solution found here: https://stackoverflow.com/questions/24963246/ace-editor-simply-re-enable-command-after-disabled-it
const setEditorCommandEnabled = (
  editor: IAceEditorProps,
  name: string,
  enabled: boolean
) => {
  const commands: ICommandManager =
    editor?.commands as unknown as ICommandManager;
  const command = commands.byName[name];
  if (!command.bindKeyOriginal) {
    command.bindKeyOriginal = command.bindKey;
  }
  command.bindKey = enabled ? command.bindKeyOriginal : null;
  commands.addCommand(command);

  // Special case for backspace and delete which will be called from
  // textarea if not handled by main commandb binding
  if (!enabled) {
    let key: any = command.bindKeyOriginal;
    if (key && typeof key === 'object') {
      key = key[commands.platform];
    }
    if (/backspace|delete/i.test(key) && commands?.bindKey) {
      commands.bindKey(key, 'null');
    }
  }
};

export type CodeEditorFieldProps = FieldWrapperPassThroughProps & {
  /**
   * The code editor field name
   */
  name: string;
  /**
   * The code editor props
   */
  editorProps?: IAceEditorProps;
  /**
   * The code editor options
   */
  editorOptions?: IAceOptions;
  /**
   * The theme of code editor
   */
  theme?: 'github' | 'eclipse';
  /**
   * The mode of code editor
   */
  mode?: string;
  /**
   * Flag to indicate if the field is disabled
   */
  disabled?: boolean;
};

export const CodeEditorField: React.FC<CodeEditorFieldProps> = ({
  name,
  editorProps,
  editorOptions = { minLines: 5, maxLines: 8, showGutter: false },
  theme = 'github',
  mode = 'json',
  disabled,
  dataTest,
  ...wrapperProps
}: CodeEditorFieldProps) => {
  const {
    control,
    formState: { errors },
  } = useFormContext();
  const maybeError = get(errors, name) as FieldError | undefined;
  const editorRef = React.useRef<AceEditor>(null);
  const [tipState, setTipState] = React.useState<'ANY' | 'ESC' | 'TAB'>('ANY');

  return (
    <FieldWrapper id={name} {...wrapperProps} error={maybeError}>
      <div
        className={clsx(
          'relative w-full max-w-xl',
          disabled ? 'cursor-not-allowed' : ''
        )}
      >
        <Controller
          name={name}
          control={control}
          render={({
            field: { value, name: controllerName, ref, onChange, onBlur },
          }) => {
            return (
              <>
                {/* Hidden input to proxy focus event from the react-hook-form controller */}
                <input
                  ref={ref}
                  onFocus={() => {
                    editorRef?.current?.editor?.focus();
                  }}
                  className="h-0 w-0 absolute"
                  tabIndex={-1}
                />
                <AceEditor
                  name={controllerName}
                  ref={editorRef}
                  value={value}
                  theme={theme}
                  mode={mode}
                  onChange={onChange}
                  onBlur={() => {
                    setTipState('ANY');
                    onBlur();
                  }}
                  onFocus={() => {
                    setTipState('ESC');
                    if (editorRef?.current?.editor) {
                      setEditorCommandEnabled(
                        editorRef?.current?.editor,
                        'indent',
                        true
                      );
                      setEditorCommandEnabled(
                        editorRef?.current?.editor,
                        'outdent',
                        true
                      );
                    }
                  }}
                  commands={[
                    {
                      name: 'Esc',
                      bindKey: { win: 'Esc', mac: 'Esc' },
                      exec: () => {
                        setTipState('TAB');
                        if (editorRef?.current?.editor) {
                          setEditorCommandEnabled(
                            editorRef?.current?.editor,
                            'indent',
                            false
                          );
                          setEditorCommandEnabled(
                            editorRef?.current?.editor,
                            'outdent',
                            false
                          );
                        }
                      },
                    },
                  ]}
                  editorProps={editorProps}
                  setOptions={editorOptions}
                  data-test={dataTest}
                  className={clsx(
                    'block relative inset-0 !w-inherit w-full max-w-xl h-code input shadow-sm rounded border border-gray-300 hover:border-gray-400 focus-within:outline-0 focus-within:ring-2 focus-within:ring-yellow-200 focus-within:border-yellow-400 placeholder-gray-500',
                    maybeError
                      ? 'border-red-600 hover:border-red-700'
                      : 'border-gray-300',
                    disabled
                      ? 'bg-gray-100 border-gray-100 pointer-events-none'
                      : 'hover:border-gray-400'
                  )}
                  data-testid={name}
                />
              </>
            );
          }}
        />
        {tipState === 'ESC' && (
          <div className="absolute bg-legacybg top-full left-1 text-gray-600 text-sm mt-1">
            Tip:{' '}
            <strong>
              Press <em>Esc</em> key
            </strong>{' '}
            then navigate with <em>Tab</em>
          </div>
        )}
        {tipState === 'TAB' && (
          <div className="absolute bg-legacybg top-full left-1 text-gray-600 text-sm mt-1">
            Tip: Press <em>Esc</em> key then{' '}
            <strong>
              navigate with <em>Tab</em>
            </strong>
          </div>
        )}
      </div>
    </FieldWrapper>
  );
};
