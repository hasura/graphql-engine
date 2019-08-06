export const sqlEscapeText = text => {
  let _text = text;

  if (_text) {
    _text = _text.replace(/'/g, "\\'");
  }

  return `E'${_text}'`;
};
