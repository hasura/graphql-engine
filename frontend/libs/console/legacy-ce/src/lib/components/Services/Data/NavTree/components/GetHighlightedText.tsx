export const GetHighlightedText = ({
  text,
  highlight,
}: {
  text: string;
  highlight: string;
}) => {
  // Split text on highlight term, include term itself into parts, ignore case
  const parts = text.split(new RegExp(`(${highlight})`, 'gi'));
  return (
    <div data-testid={text}>
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
