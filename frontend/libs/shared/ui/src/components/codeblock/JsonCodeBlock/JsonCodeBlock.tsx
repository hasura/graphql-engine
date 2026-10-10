import { useMemo } from 'react';
import hljs from 'highlight.js/lib/core';
import json from 'highlight.js/lib/languages/json';
import { CodeBlock, CodeBlockProps } from '../CodeBlock/CodeBlock';

hljs.registerLanguage('json', json);

export type JsonCodeBlockProps = Omit<
  CodeBlockProps,
  'highlightedHtml' | 'text'
> & {
  /**
   * The value to display. Strings are shown as-is; anything else is passed
   * through `JSON.stringify`.
   */
  value: unknown;
  /**
   * Number of spaces used to indent the stringified JSON.
   * @default 2
   */
  indent?: number;
};

export const JsonCodeBlock = ({
  value,
  indent = 2,
  ...rest
}: JsonCodeBlockProps) => {
  const formatted =
    typeof value === 'string' ? value : JSON.stringify(value, null, indent);

  const highlighted = useMemo(() => {
    try {
      return hljs.highlight(formatted, {
        language: 'json',
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
      copyButtonLabel="Copy JSON"
      {...rest}
    />
  );
};
