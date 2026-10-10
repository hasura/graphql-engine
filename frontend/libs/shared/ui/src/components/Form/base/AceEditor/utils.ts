// eslint-disable-file import/no-extraneous-dependencies

import { Ace } from 'ace-builds';
import { ICommandManager } from 'react-ace';

export const ACE_EDITOR_THEME = 'chrome';
// Dark counterpart used when the app appearance is dark and no explicit theme is
// passed. The matching theme module is imported in AceEditor/index.tsx.
export const ACE_EDITOR_THEME_DARK = 'tomorrow_night';
export const ACE_EDITOR_FONT_SIZE = 14;

export const getLanguageModeFromExtension = (extension: string) => {
  switch (extension) {
    case 'ts':
      return 'typescript';
    case 'go':
      return 'golang';
    case 'kt':
      return 'kotlin';
    case 'py':
      return 'python';
    case 'java':
      return 'java';
    case 'ruby':
      return 'ruby';
    case 'sql':
      return 'sql';
    case 'graphql':
    case 'gql':
      return 'graphqlschema';
    default:
      return 'javascript';
  }
};

// Allows to integrate the code editor in a form: by default, tab key adds a
// tab character to the editor content, here, we want to disable this behavior
// and allow to navigate with the tab key. This is done by setting the command.
// Solution found here: https://stackoverflow.com/questions/24963246/ace-editor-simply-re-enable-command-after-disabled-it
export const setEditorCommandEnabled = (
  editor: Ace.Editor,
  name: string,
  enabled: boolean,
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
