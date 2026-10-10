import { useMemo } from 'react';
import hljs from 'highlight.js/lib/core';
import graphql from 'highlight.js/lib/languages/graphql';
import { CodeBlock, CodeBlockProps } from '../CodeBlock/CodeBlock';

hljs.registerLanguage('graphql', graphql);

export type GraphqlCodeBlockProps = Omit<CodeBlockProps, 'highlightedHtml'>;

export const GraphqlCodeBlock = ({ text, ...rest }: GraphqlCodeBlockProps) => {
  const highlighted = useMemo(() => {
    try {
      return hljs.highlight(text, {
        language: 'graphql',
        ignoreIllegals: true,
      }).value;
    } catch {
      return undefined;
    }
  }, [text]);

  return (
    <CodeBlock
      highlightedHtml={highlighted}
      copyButtonLabel="Copy GraphQL"
      text={text}
      {...rest}
    />
  );
};
