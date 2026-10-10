import { CodeBlock } from '../CodeBlock/CodeBlock';

export type PlainCodeBlockProps = {
  /**
   * The text to display, unformatted and unhighlighted.
   */
  value: string;
  /**
   * Hides the copy-to-clipboard button.
   * @default false
   */
  hideCopyButton?: boolean;
  /**
   * Caps the block height and scrolls once content overflows.
   * @default true
   */
  scrollable?: boolean;
  className?: string;
};

export const PlainCodeBlock = ({
  value,
  hideCopyButton = false,
  scrollable = true,
  className,
}: PlainCodeBlockProps) => (
  <CodeBlock
    text={value}
    hideCopyButton={hideCopyButton}
    scrollable={scrollable}
    className={className}
  />
);
