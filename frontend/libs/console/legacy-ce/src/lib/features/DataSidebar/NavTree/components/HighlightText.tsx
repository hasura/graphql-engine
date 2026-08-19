import clsx from 'clsx';
import escapeRegExp from 'lodash/escapeRegExp';

export const HighlightText = ({
  text,
  highlightedText,
  className,
}: {
  text: string;
  highlightedText: string;
  className?: string;
}) => {
  const containerProps = {
    title: text, // so overflowed elements will have their text visible on hover
    'data-testid': text,
    className: clsx(className),
  };

  if (!highlightedText) return <div {...containerProps}>{text}</div>;

  // the search term is typed by the user, so it has to be escaped before being
  // used in a RegExp, otherwise metacharacters like `(` throw a SyntaxError
  const splitText = text
    .split(new RegExp(`(${escapeRegExp(highlightedText)})`, 'gi'))
    .filter(Boolean);

  return (
    <div {...containerProps}>
      {splitText.map((str, index) => {
        if (str.toLowerCase() === highlightedText.toLowerCase()) {
          return (
            <strong className="underline" key={index}>
              {str}
            </strong>
          );
        }
        return str;
      })}
    </div>
  );
};
