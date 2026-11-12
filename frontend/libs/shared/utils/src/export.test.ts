import { buildCSV, convertRowsToCSV, convertRowsToJSON } from './export';

describe('convertRowsToCSV', () => {
  describe('when data is provided', () => {
    it('returns a CSV', () => {
      const rows: Record<string, unknown>[] = [
        { id: 1, name: 'john' },
        { id: 2, name: 'jane' },
      ];
      expect(convertRowsToCSV(rows)).toBe(
        `data:text/csv;charset=utf-8,id%2Cname%0A1%2Cjohn%0A2%2Cjane`,
      );
    });

    it('does not add apostrophes or quotes around plain values', () => {
      const csv = decodeURIComponent(
        convertRowsToCSV([{ column_a: 'value1', column_b: 'value2' }]),
      );
      expect(csv).toBe(
        'data:text/csv;charset=utf-8,column_a,column_b\nvalue1,value2',
      );
    });

    it('quotes values with commas, quotes or line breaks, and keeps #', () => {
      const csv = decodeURIComponent(
        convertRowsToCSV([
          {
            a: 'x, y',
            b: 'say "hi"',
            c: 'line 1\nline 2',
            d: 'blah #blah',
            e: null,
            f: { k: 'v' },
          },
        ]),
      );
      expect(csv).toBe(
        'data:text/csv;charset=utf-8,a,b,c,d,e,f\n' +
          '"x, y","say ""hi""","line 1\nline 2",blah #blah,,"{""k"":""v""}"',
      );
    });

    it('writes every row in the order of the header', () => {
      const csv = decodeURIComponent(
        convertRowsToCSV([
          { id: 1, name: 'john' },
          { name: 'jane', id: 2 },
        ]),
      );
      expect(csv).toBe('data:text/csv;charset=utf-8,id,name\n1,john\n2,jane');
    });
  });

  describe('when data is not provided', () => {
    it('returns an empty CSV', () => {
      expect(convertRowsToCSV(undefined)).toBe(`data:text/csv;charset=utf-8,`);
    });
  });
});

describe('buildCSV', () => {
  const rows = [
    { id: 1, name: 'john; doe', note: null },
    { id: 2, name: 'jane', note: 'ok' },
  ];

  it('uses the given delimiter, quoting cells that contain it', () => {
    expect(buildCSV(rows, { delimiter: ';' })).toBe(
      'id;name;note\n1;"john; doe";\n2;jane;ok',
    );
  });

  it('supports tab and multi-character delimiters', () => {
    expect(buildCSV([{ a: 'x\ty', b: 'z' }], { delimiter: '\t' })).toBe(
      'a\tb\n"x\ty"\tz',
    );
    expect(buildCSV([{ a: 'x||y', b: 'z' }], { delimiter: '||' })).toBe(
      'a||b\n"x||y"||z',
    );
  });

  it('can leave out the header row', () => {
    expect(buildCSV(rows, { includeHeader: false })).toBe(
      '1,john; doe,\n2,jane,ok',
    );
  });

  it('can start with a UTF-8 BOM and use CRLF line endings', () => {
    expect(
      buildCSV([{ name: 'Đà Nẵng' }], { includeBOM: true, lineEnding: 'CRLF' }),
    ).toBe('\uFEFFname\r\nĐà Nẵng');
  });

  it('can quote every cell', () => {
    expect(buildCSV([{ id: 1, name: 'a' }], { quoteAllCells: true })).toBe(
      '"id","name"\n"1","a"',
    );
  });

  it('writes nulls as the given text', () => {
    expect(buildCSV(rows, { nullValue: 'NULL' })).toBe(
      'id,name,note\n1,john; doe,NULL\n2,jane,ok',
    );
  });

  it('escapes text that looks like a formula, but not numbers', () => {
    const formulaRows = [
      { a: '=SUM(A1:A2)', b: '+1', c: '-x', d: '@x', e: -5, f: 'a=b' },
    ];
    expect(buildCSV(formulaRows, { escapeFormulas: true })).toBe(
      "a,b,c,d,e,f\n'=SUM(A1:A2),'+1,'-x,'@x,-5,a=b",
    );
    expect(buildCSV(formulaRows)).toBe(
      'a,b,c,d,e,f\n=SUM(A1:A2),+1,-x,@x,-5,a=b',
    );
  });

  it('writes the header from the fallback columns when there are no rows', () => {
    expect(buildCSV([], {}, ['id', 'name'])).toBe('id,name');
  });

  it('prefers the keys of the rows over the fallback columns', () => {
    expect(buildCSV([{ id: 1 }], {}, ['other'])).toBe('id\n1');
  });
});

describe('convertRowsToJSON', () => {
  describe('when data is provided', () => {
    it('returns a JSON', () => {
      const rows = [
        { id: 1, name: 'john' },
        { id: 2, name: 'jane' },
      ];
      expect(convertRowsToJSON(rows)).toBe(
        `data:application/json;charset=utf-8;,%5B%0A%20%20%7B%0A%20%20%20%20%22id%22%3A%201%2C%0A%20%20%20%20%22name%22%3A%20%22john%22%0A%20%20%7D%2C%0A%20%20%7B%0A%20%20%20%20%22id%22%3A%202%2C%0A%20%20%20%20%22name%22%3A%20%22jane%22%0A%20%20%7D%0A%5D`,
      );
    });
  });
});
