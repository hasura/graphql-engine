import { render } from '@testing-library/react';
import { Theme } from '@radix-ui/themes';
import { vi } from 'vitest';
import { SqlCodeBlock } from './SqlCodeBlock';

// Make the formatter output deterministic and dependent on the resolved SQL
// dialect so the test can observe that `language` participates in the memo.
vi.mock('sql-formatter', () => ({
  format: (text: string, { language }: { language: string }) =>
    `${language}::${text}`,
}));

describe('SqlCodeBlock', () => {
  it('formats the text on initial render using the given driver', () => {
    const { container } = render(
      <Theme>
        <SqlCodeBlock text="SELECT 1" language="bigquery" />
      </Theme>,
    );

    // adaptSqlLanguage('bigquery') === 'bigquery'
    expect(container.textContent).toContain('bigquery::SELECT 1');
  });

  it('re-formats when the language prop changes', () => {
    const { container, rerender } = render(
      <Theme>
        <SqlCodeBlock text="SELECT 1" language="bigquery" />
      </Theme>,
    );

    expect(container.textContent).toContain('bigquery::SELECT 1');

    rerender(
      <Theme>
        <SqlCodeBlock text="SELECT 1" language="postgres" />
      </Theme>,
    );

    // adaptSqlLanguage('postgres') === 'plsql'
    expect(container.textContent).toContain('plsql::SELECT 1');
    expect(container.textContent).not.toContain('bigquery::SELECT 1');
  });
});
