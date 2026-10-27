import { IAnnotation } from 'react-ace';

/** Points the parse error at its line, from the position the error reports. */
export const getJsonErrorAnnotation = (
  text: string,
  error: unknown,
): IAnnotation => {
  const message = (error instanceof Error ? error.message : String(error))
    // V8 quotes the whole input in some messages
    .replace(/, ".*" is not valid JSON$/s, '');
  const lineColumn = message.match(/line (\d+) column (\d+)/);
  const position = message.match(/position (\d+)/);

  const pointAt = (offset: number) => {
    const lines = text.slice(0, offset).split('\n');
    return { row: lines.length - 1, column: lines[lines.length - 1].length };
  };

  // without a position (e.g. "Unexpected token"), the first line is marked
  let location = { row: 0, column: 0 };
  if (lineColumn) {
    location = {
      row: Number(lineColumn[1]) - 1,
      column: Number(lineColumn[2]) - 1,
    };
  } else if (position) {
    location = pointAt(Number(position[1]));
  } else if (/end of JSON input/.test(message)) {
    location = pointAt(text.length);
  }

  return { ...location, type: 'error', text: message };
};

/** The annotations for the parse error of `text`, if it isn't valid JSON. */
export const getJsonAnnotations = (
  text: string,
  { allowEmpty = false } = {},
): IAnnotation[] => {
  if (allowEmpty && !text.trim()) return [];
  try {
    JSON.parse(text);
    return [];
  } catch (error) {
    return [getJsonErrorAnnotation(text, error)];
  }
};
