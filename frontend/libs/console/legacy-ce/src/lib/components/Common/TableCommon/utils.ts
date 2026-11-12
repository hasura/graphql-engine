import { isNotDefined, isObject } from '@hasura/shared/utils';

// values longer than this are measured by their beginning only: in any
// visible font, that is already wider than the widest column
const MAX_MEASURED_LENGTH = 1000;

// measuring text is the costly part: of a long column, only the values with
// the most characters are measured, as the widest value is one of them in
// all but contrived cases (e.g. `iiii` against `WW`)
const MAX_MEASURED_VALUES = 100;

const toText = (content: unknown): string => {
  if (isNotDefined(content)) return 'NULL';
  if (isObject(content)) return JSON.stringify(content, null, 4);
  return String(content);
};

// return column width to fit the header and all of the column's values
export const getColWidth = (
  header: string,
  values: unknown[] = [],
  MAX_WIDTH = 600,
  HEADER_PADDING = 62,
  CONTENT_PADDING = 18,
  HEADER_FONT = 'bold 16px Gudea',
  CONTENT_FONT = '14px Gudea',
) => {
  // Doesn't work well with non-monospace fonts
  // const CHAR_WIDTH = 8;
  // return text.length * CHAR_WIDTH;

  // if given, use cached canvas for better performance
  // else, create new canvas
  const canvas: HTMLCanvasElement =
    (getColWidth as any).canvas ||
    ((getColWidth as any).canvas = document.createElement('canvas'));
  const context = canvas.getContext('2d') as CanvasRenderingContext2D;

  context.font = HEADER_FONT;
  const headerCellWidth = context.measureText(header).width + HEADER_PADDING;
  if (headerCellWidth >= MAX_WIDTH) return MAX_WIDTH;

  // longest first, so a column that reaches the maximum width does so early
  const texts = values
    .filter((content) => content !== null)
    .map(toText)
    .sort((a, b) => b.length - a.length)
    .slice(0, MAX_MEASURED_VALUES);

  // setting the font is costly, so it is set once for all the values
  context.font = CONTENT_FONT;
  const maxContentWidth = MAX_WIDTH - CONTENT_PADDING;
  let contentWidth = 0;
  for (const contentString of texts) {
    contentWidth = Math.max(
      contentWidth,
      context.measureText(contentString.slice(0, MAX_MEASURED_LENGTH)).width,
    );
    // the column is as wide as it can be: no other value can change that
    if (contentWidth >= maxContentWidth) return MAX_WIDTH;
  }

  return Math.max(contentWidth + CONTENT_PADDING, headerCellWidth);
};
