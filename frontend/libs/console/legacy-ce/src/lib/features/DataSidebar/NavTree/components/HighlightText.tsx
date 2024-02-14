import clsx from 'clsx';

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

  const splitText = text
    .split(new RegExp(`(${highlightedText})`, 'gi'))
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
