import { useRef, useState } from 'react';
import clsx from 'clsx';
import { Card, CardProps, Code, CodeProps, Flex } from '@radix-ui/themes';
import { FaRegCheckCircle, FaRegCopy } from 'react-icons/fa';
import { IconButton } from '../../Button';

export type CodeBlockProps = {
  /**
   * The already-formatted text to display.
   */
  text: string;
  /**
   * Pre-highlighted HTML (via `dangerouslySetInnerHTML`). When `undefined`,
   * `text` is rendered as plain, unhighlighted content.
   */
  highlightedHtml?: string;
  /**
   * aria-label for the copy button.
   * @default 'Copy to clipboard'
   */
  copyButtonLabel?: string;
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

  size?: CodeProps['size'];
  variant?: CardProps['variant'];
  height?: string;
};

/**
 * Tailwind colors for the `hljs-*` token classes highlight.js emits,
 * scoped via arbitrary-variant selectors so no separate highlight.js theme
 * stylesheet needs to be imported.
 *
 * Dark mode uses the Tomorrow Night palette, matching the AceEditor's dark
 * theme (`ACE_EDITOR_THEME_DARK`), with tokens mapped to the same colors Ace
 * gives their equivalents (e.g. JSON keys are `ace_variable` -> #CC6666).
 */
export const hljsTokenClassNames = clsx(
  '[&_.hljs-attr]:text-sky-700',
  '[&_.hljs-string]:text-emerald-700',
  '[&_.hljs-number]:text-amber-700',
  '[&_.hljs-literal]:text-purple-700',
  '[&_.hljs-punctuation]:text-gray-500',
  '[&_.hljs-keyword]:text-purple-700',
  '[&_.hljs-title]:text-sky-700',
  '[&_.hljs-name]:text-sky-700',
  '[&_.hljs-comment]:text-gray-700',
  // Tomorrow Night
  'dark:[&_.hljs-attr]:text-[#CC6666]',
  'dark:[&_.hljs-name]:text-[#CC6666]',
  'dark:[&_.hljs-variable]:text-[#CC6666]',
  'dark:[&_.hljs-string]:text-[#B5BD68]',
  'dark:[&_.hljs-symbol]:text-[#B5BD68]',
  'dark:[&_.hljs-number]:text-[#DE935F]',
  'dark:[&_.hljs-literal]:text-[#DE935F]',
  'dark:[&_.hljs-keyword]:text-[#B294BB]',
  'dark:[&_.hljs-meta]:text-[#B294BB]',
  'dark:[&_.hljs-title]:text-[#81A2BE]',
  // `hljs-built_in`: Tailwind turns `_` in arbitrary variants into a space, and
  // an escaped `\\_` is extracted differently from its runtime value, so match
  // the class by prefix instead.
  'dark:[&_[class^=hljs-built]]:text-[#81A2BE]',
  'dark:[&_.hljs-operator]:text-[#8ABEB7]',
  'dark:[&_.hljs-punctuation]:text-[#C5C8C6]',
  'dark:[&_.hljs-comment]:text-[#969896]',
);

const iconClassName = 'w-3.5 h-3.5';

/**
 * `<Code>` is rendered as a block so its own line-height alone sets the row
 * height (as an inline element, the taller of the wrapper's line-height and
 * Radix's fixed per-size line-height wins). The value is set inline because
 * Radix's unlayered `.rt-Code` size rules would override a Tailwind
 * `leading-*` class. Unitless, so it scales with every `size`.
 */
const codeStyle = { lineHeight: 1.4 };

export const CodeBlock = ({
  text,
  highlightedHtml,
  copyButtonLabel = 'Copy to clipboard',
  hideCopyButton = false,
  scrollable,
  className,
  size = '2',
  variant,
  height,
}: CodeBlockProps) => {
  const [showCopiedConfirmation, setShowCopiedConfirmation] = useState(false);
  const copyTimer = useRef<NodeJS.Timeout>(undefined);

  const handleCopy = () => {
    if (copyTimer.current) {
      clearTimeout(copyTimer.current);
    }

    navigator.clipboard.writeText(text);

    setShowCopiedConfirmation(true);
    copyTimer.current = setTimeout(() => {
      setShowCopiedConfirmation(false);
    }, 1500);
  };

  return (
    <Card variant={variant} size="1" className={clsx('relative', className)}>
      {!hideCopyButton && (
        <Flex className="absolute top-1.5 right-1.5" align="center">
          <IconButton
            type="button"
            variant="ghost"
            size="1"
            color={showCopiedConfirmation ? 'green' : 'gray'}
            aria-label={copyButtonLabel}
            data-testid="code-block-copy-button"
            onClick={handleCopy}
          >
            {showCopiedConfirmation ? (
              <FaRegCheckCircle className={iconClassName} />
            ) : (
              <FaRegCopy className={iconClassName} />
            )}
          </IconButton>
        </Flex>
      )}
      <div
        className={clsx(
          'whitespace-pre-wrap break-all rounded-md',
          hljsTokenClassNames,
          scrollable && 'overflow-auto',
        )}
        style={{
          height: height || (scrollable ? '200px' : undefined),
        }}
      >
        {highlightedHtml !== undefined ? (
          <Code
            size={size}
            variant="ghost"
            className="block"
            style={codeStyle}
            dangerouslySetInnerHTML={{ __html: highlightedHtml }}
          />
        ) : (
          <Code size={size} variant="ghost" className="block" style={codeStyle}>
            {text}
          </Code>
        )}
      </div>
    </Card>
  );
};
