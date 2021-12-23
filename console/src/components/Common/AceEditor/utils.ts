// eslint-disable-file import/no-extraneous-dependencies

import 'ace-builds/src-noconflict/theme-eclipse';
import 'ace-builds/src-noconflict/mode-graphqlschema';
import 'ace-builds/src-noconflict/mode-sql';
import 'ace-builds/src-noconflict/ext-searchbox';

export const ACE_EDITOR_THEME = 'eclipse';
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
    default:
      return 'javascript';
  }
};
