const urlRegex = /https?:\/\/[^\s]+/g;

export const createTextWithLinks = (text: string): JSX.Element => {
  const parts = text.split(urlRegex);

  // Find all the links in the text
  const links = Array.from(text.matchAll(urlRegex)).map(match => match[0]);

  const result: JSX.Element[] = [];

  // Iterate through the parts and alternate between adding regular text and links
  for (let i = 0; i < parts.length; i++) {
    if (parts[i]) {
      result.push(<span key={`text-${i}`}>{parts[i]}</span>);
    }
    if (links[i]) {
      result.push(
        <a
          key={`link-${i}`}
          href={links[i]}
          target="_blank"
          rel="noopener noreferrer"
        >
          {links[i]}
        </a>
      );
    }
  }

  return <div>{result}</div>;
};
