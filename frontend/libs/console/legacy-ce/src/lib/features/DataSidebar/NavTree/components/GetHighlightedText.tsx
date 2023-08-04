import clsx from 'clsx';

export const GetHighlightedText = ({
  text,
  highlight,
  className,
}: {
  text: string;
  highlight: string;
  className?: string;
}) => {
  // Split text on highlight term, include term itself into parts, ignore case
  const parts = text.split(new RegExp(`(${highlight})`, 'gi'));
  return (
    <div className={clsx(className)} data-testid={text}>
      {parts.map((part, index) =>
        part.toLowerCase() === highlight.toLowerCase() ? (
          <b key={index}>{part}</b>
        ) : (
          part
        )
      )}
    </div>
  );
};
