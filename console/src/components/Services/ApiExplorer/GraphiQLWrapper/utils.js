import { getLSItem } from '../../../../utils/localStorage';

export const getGraphiQLQueryFromLocalStorage = () => {
  return getLSItem('graphiql:query');
};

export const clearCodeMirrorHints = () => {
  const cmNodes = document.querySelectorAll('.CodeMirror-hints.graphiql');

  if (cmNodes.length > 0) {
    cmNodes.forEach(cm => {
      cm.remove();
    });
  }
};

export const setQueryVariableSectionHeight = () => {
  const variableEditor = document.querySelectorAll('.variable-editor');
  if (variableEditor && variableEditor.length > 0) {
    variableEditor[0].style.height = '120px';
  }
};

export const copyToClipboard = copyString => {
  const queryTextArea = document.createElement('textarea');
  queryTextArea.value = copyString;
  queryTextArea.setAttribute('readonly', '');
  queryTextArea.style = { position: 'absolute', left: '-9999px' };
  document.body.appendChild(queryTextArea);
  queryTextArea.select();
  document.execCommand('copy');
  document.body.removeChild(queryTextArea);
};
