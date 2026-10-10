import { useMemo } from 'react';
import hljs from 'highlight.js/lib/core';
import sql from 'highlight.js/lib/languages/sql';
import { format, SqlLanguage } from 'sql-formatter';
import { CodeBlock, CodeBlockProps } from '../CodeBlock/CodeBlock';
import { SupportedDriver } from '@hasura/shared/types';

hljs.registerLanguage('sql', sql);

export type SqlCodeBlockProps = Omit<CodeBlockProps, 'highlightedHtml'> & {
  language?: SupportedDriver;
};

const adaptSqlLanguage = (driver: SupportedDriver | undefined): SqlLanguage => {
  switch (driver) {
    case 'bigquery':
      return 'bigquery';
    case 'mariadb':
      return 'mariadb';
    case 'mysql':
      return 'mysql';
    case 'mssql':
      return 'mysql';
    case 'snowflake':
      return 'snowflake';
    case 'sqlite':
      return 'sqlite';
    case 'redshift':
    case 'oracle':
    case 'alloy':
    case 'postgres':
    case 'citus':
    case 'cockroach':
    case 'athena':
    default:
      return 'plsql';
  }
};

export const SqlCodeBlock = ({
  text,
  language,
  ...props
}: SqlCodeBlockProps) => {
  const formatted = useMemo(() => {
    if (!text?.trim()) {
      return '';
    }

    try {
      return format(text, {
        language: adaptSqlLanguage(language),
      });
    } catch {
      return text;
    }
  }, [text]);

  const highlighted = useMemo(() => {
    try {
      return hljs.highlight(formatted, {
        language: 'sql',
        ignoreIllegals: true,
      }).value;
    } catch {
      return undefined;
    }
  }, [formatted]);

  return (
    <CodeBlock
      text={formatted}
      highlightedHtml={highlighted}
      copyButtonLabel="Copy SQL"
      {...props}
    />
  );
};
