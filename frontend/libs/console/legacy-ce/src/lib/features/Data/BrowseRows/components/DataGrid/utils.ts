import { OrderBy } from '@hasura/shared/types';
import { ColumnSort } from '@tanstack/react-table';

export const transformToOrderByClause = (sorts: ColumnSort[]): OrderBy[] => {
  return sorts.map((sort) => ({
    column: sort.id,
    type: sort.desc ? 'desc' : 'asc',
  }));
};

// Longest text shown in a cell of the rows table. Very large values (e.g. a
// bytea column holding a file) would otherwise be rendered in full for every
// visible row and freeze the page; the row dialog still shows the full value.
export const MAX_CELL_TEXT_LENGTH = 1000;

const stringifyValue = (data: unknown): string | undefined => {
  if (
    typeof data === 'string' ||
    typeof data === 'number' ||
    typeof data === 'bigint'
  ) {
    return String(data);
  }

  try {
    // bigints nested in objects/arrays would otherwise make JSON.stringify throw
    return JSON.stringify(data, (_key, value) =>
      typeof value === 'bigint' ? value.toString() : value,
    );
  } catch {
    // e.g. a cyclic value; never let a single cell break the whole grid
    return String(data);
  }
};

// Pass `maxLength: Infinity` for values that must stay whole, such as
// primary keys, which identify the row.
export const formatCellValue = (
  data: unknown,
  maxLength = MAX_CELL_TEXT_LENGTH,
) => {
  const text = stringifyValue(data);

  if (text === undefined || text.length <= maxLength) {
    return text;
  }

  return `${text.slice(0, maxLength)}… (${text.length.toLocaleString('en-US')} characters)`;
};

const HEX_PREFIX = /^(\\x|0x)/i;
const HEX = /^[0-9a-f]*$/i;
const BASE64 = /^[A-Za-z0-9+/]*={0,2}$/;

const isBase64 = (value: string) =>
  value.length % 4 === 0 && BASE64.test(value);

/**
 * Decodes a binary value as returned by GraphQL: hex with a `\x` (Postgres)
 * or `0x` prefix, or base64 (e.g. BigQuery). Anything else is kept as text.
 */
export const decodeBinaryValue = (value: string): Uint8Array => {
  const hex = value.replace(HEX_PREFIX, '');
  if (hex !== value && hex.length % 2 === 0 && HEX.test(hex)) {
    const bytes = new Uint8Array(hex.length / 2);
    for (let i = 0; i < bytes.length; i++) {
      bytes[i] = parseInt(hex.slice(i * 2, i * 2 + 2), 16);
    }
    return bytes;
  }

  if (isBase64(value)) {
    return Uint8Array.from(atob(value), (char) => char.charCodeAt(0));
  }

  return new TextEncoder().encode(value);
};

/**
 * The size in bytes of a binary value, from its encoded length only, so it is
 * cheap enough to show for every cell.
 */
export const getBinaryValueSize = (value: string) => {
  if (HEX_PREFIX.test(value)) return Math.floor((value.length - 2) / 2);

  if (isBase64(value)) {
    return (value.length / 4) * 3 - (value.match(/=+$/)?.[0].length ?? 0);
  }

  return value.length;
};

export const formatByteSize = (bytes: number) => {
  if (bytes < 1024) return `${bytes} B`;
  if (bytes < 1024 * 1024) return `${(bytes / 1024).toFixed(1)} KB`;
  return `${(bytes / (1024 * 1024)).toFixed(1)} MB`;
};

export const downloadBinaryValue = (fileName: string, value: string) => {
  const url = URL.createObjectURL(
    new Blob([decodeBinaryValue(value) as BlobPart], {
      type: 'application/octet-stream',
    }),
  );
  const link = document.createElement('a');
  link.href = url;
  link.download = fileName;
  document.body.appendChild(link);
  link.click();
  link.remove();
  URL.revokeObjectURL(url);
};
