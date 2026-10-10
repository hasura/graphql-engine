import { useMemo } from 'react';
import hljs from 'highlight.js/lib/core';
import javascript from 'highlight.js/lib/languages/javascript';
import { CodeBlock, CodeBlockProps } from './CodeBlock';

hljs.registerLanguage('javascript', javascript);

export type JavascriptCodeBlockProps = Omit<CodeBlockProps, 'highlightedHtml'>;

export const JavascriptCodeBlock = ({
  text,
  ...rest
}: JavascriptCodeBlockProps) => {
  const highlighted = useMemo(() => {
    try {
      return hljs.highlight(text, {
        language: 'javascript',
        ignoreIllegals: true,
      }).value;
    } catch {
      return undefined;
    }
  }, [text]);

  return (
    <CodeBlock
      highlightedHtml={highlighted}
      copyButtonLabel="Copy"
      text={text}
      {...rest}
    />
  );
};
