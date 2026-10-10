import { forwardRef, useState } from 'react';
import BaseEditor, { IAceEditorProps, IAceOptions } from 'react-ace';
import {
  ACE_EDITOR_THEME,
  ACE_EDITOR_THEME_DARK,
  ACE_EDITOR_FONT_SIZE,
  setEditorCommandEnabled,
} from './utils';
import { useOptionalAppearance } from '../../../../theme/ThemeProvider';
import { Tooltip } from '../../../Tooltip';
import { Strong, Text } from '@radix-ui/themes';
import clsx from 'clsx';

import 'ace-builds/src-noconflict/ext-searchbox';
import 'ace-builds/src-noconflict/ext-error_marker';
import 'ace-builds/src-noconflict/ext-beautify';
import 'ace-builds/src-noconflict/mode-html';
import 'ace-builds/src-noconflict/mode-markdown';
import 'ace-builds/src-noconflict/mode-json';
import 'ace-builds/src-noconflict/mode-javascript';
import 'ace-builds/src-noconflict/mode-typescript';
import 'ace-builds/src-noconflict/mode-golang';
import 'ace-builds/src-noconflict/mode-kotlin';
import 'ace-builds/src-noconflict/mode-python';
import 'ace-builds/src-noconflict/mode-java';
import 'ace-builds/src-noconflict/mode-ruby';
import 'ace-builds/src-noconflict/theme-chrome';
import 'ace-builds/src-noconflict/theme-tomorrow_night';
import 'ace-builds/src-noconflict/mode-graphqlschema';
import 'ace-builds/src-noconflict/mode-sql';
import 'ace-builds/src-noconflict/mode-yaml';

export { getLanguageModeFromExtension } from './utils';

export type AceEditorProps = Omit<IAceEditorProps, 'readOnly'> & {
  disabled?: boolean;
  invalid?: boolean;
};

export type AceEditorRef = BaseEditor;

const DEFAULT_EDITOR_OPTIONS: IAceOptions = {
  showLineNumbers: true,
  useWorker: false,
  showGutter: true,
};

export const AceEditor = forwardRef<AceEditorRef, AceEditorProps>(
  (
    {
      className,
      theme: themeProp,
      fontSize = ACE_EDITOR_FONT_SIZE,
      showGutter = true,
      tabSize = 2,
      setOptions,
      disabled,
      invalid,
      onFocus,
      onBlur,
      ...props
    },
    ref,
  ) => {
    const [tipState, setTipState] = useState<'ANY' | 'ESC' | 'TAB'>('ANY');

    // When the caller does not pin a theme, follow the console appearance so the
    // editor recolors live on a dark-mode toggle (react-ace re-themes in place,
    // no remount, so editor content is preserved).
    const appearance = useOptionalAppearance()?.appearance ?? 'light';
    const theme =
      themeProp ??
      (appearance === 'dark' ? ACE_EDITOR_THEME_DARK : ACE_EDITOR_THEME);

    return (
      <div className="relative">
        <BaseEditor
          ref={
            ref
              ? (editor) => {
                  typeof ref === 'function'
                    ? ref(editor)
                    : (ref.current = editor);

                  if (editor?.editor?.renderer) {
                    editor.editor.renderer.setCursorStyle('display: none');
                  }
                }
              : undefined
          }
          theme={theme}
          fontSize={fontSize}
          showGutter={showGutter}
          tabSize={tabSize}
          setOptions={{
            ...DEFAULT_EDITOR_OPTIONS,
            ...setOptions,
          }}
          readOnly={disabled}
          highlightActiveLine={!disabled}
          onBlur={(ev, editor) => {
            setTipState('ANY');
            onBlur?.(ev, editor);
          }}
          onFocus={(ev, editor) => {
            setTipState('ESC');
            if (editor) {
              setEditorCommandEnabled(editor, 'indent', true);
              setEditorCommandEnabled(editor, 'outdent', true);
            }
            onFocus?.(ev, editor);
          }}
          commands={[
            {
              name: 'Esc',
              bindKey: { win: 'Esc', mac: 'Esc' },
              exec: (editor) => {
                setTipState('TAB');
                if (editor) {
                  setEditorCommandEnabled(editor, 'indent', false);
                  setEditorCommandEnabled(editor, 'outdent', false);
                }
              },
            },
          ]}
          className={clsx(
            'block relative inset-0 w-full focus-within:outline-0',
            invalid ? 'border-red-600 hover:border-red-700' : 'border-gray-300',
            disabled
              ? 'bg-gray-200 border-gray-200 hover:border-gray-200 focus-within:ring-0 focus-within:border-gray-200'
              : 'hover:border-gray-400',
            className,
          )}
          {...props}
        />

        <Tooltip
          side="bottom"
          open={tipState !== 'ANY'}
          content={
            tipState === 'ESC' ? (
              <Text>
                Tip:{' '}
                <Strong>
                  Press <em>Esc</em> key
                </Strong>{' '}
                then navigate with <em>Tab</em>
              </Text>
            ) : tipState === 'TAB' ? (
              <Text>
                Tip: Press <em>Esc</em> key then{' '}
                <Strong>
                  navigate with <em>Tab</em>
                </Strong>
              </Text>
            ) : null
          }
        >
          <div className="absolute bottom-0 right-0 left-0 h-0 -z-1">
            &nbsp;
          </div>
        </Tooltip>
      </div>
    );
  },
);
