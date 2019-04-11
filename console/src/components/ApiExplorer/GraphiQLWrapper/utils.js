export const getGraphiQLQueryFromLocalStorage = () => {
  return window.localStorage.getItem('graphiql:query');
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
