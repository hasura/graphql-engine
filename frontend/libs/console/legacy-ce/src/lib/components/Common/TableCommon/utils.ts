import { isNotDefined, isObject } from '@hasura/shared/utils';

// return column width to fit all rows content
export const getColWidth = (
  header: string,
  contentRows: Record<string, any>[] = [],
  MAX_WIDTH = 600,
  HEADER_PADDING = 62,
  CONTENT_PADDING = 18,
  HEADER_FONT = 'bold 16px Gudea',
  CONTENT_FONT = '14px Gudea',
) => {
  const getTextWidth = (text: string, font: string) => {
    // Doesn't work well with non-monospace fonts
    // const CHAR_WIDTH = 8;
    // return text.length * CHAR_WIDTH;

    // if given, use cached canvas for better performance
    // else, create new canvas
    const canvas =
      (getTextWidth as any).canvas ||
      ((getTextWidth as any).canvas = document.createElement('canvas'));

    const context = canvas.getContext('2d');
    context.font = font;

    const metrics = context.measureText(text);
    return metrics.width;
  };

  let maxContentWidth = 0;
  for (let i = 0; i < contentRows.length; i++) {
    if (contentRows[i] !== undefined && contentRows[i][header] !== null) {
      const content = contentRows[i][header];

      let contentString;
      if (isNotDefined(content)) {
        contentString = 'NULL';
      } else if (isObject(content)) {
        contentString = JSON.stringify(content, null, 4);
      } else {
        contentString = content.toString();
      }

      const currLength = getTextWidth(contentString, CONTENT_FONT);

      if (currLength > maxContentWidth) {
        maxContentWidth = currLength;
      }
    }
  }

  const maxContentCellWidth = maxContentWidth + CONTENT_PADDING;

  const headerCellWidth = getTextWidth(header, HEADER_FONT) + HEADER_PADDING;

  return Math.min(MAX_WIDTH, Math.max(maxContentCellWidth, headerCellWidth));
};
