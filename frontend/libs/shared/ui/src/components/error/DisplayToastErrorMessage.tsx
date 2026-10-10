import { JsonCodeBlock } from '../codeblock';
import { CodeBlock } from '../codeblock/CodeBlock/CodeBlock';

export const DisplayToastErrorMessage = ({ message }: { message: unknown }) => {
  return (
    <div className="overflow-hidden py-1.5">
      {typeof message === 'object' ? (
        <JsonCodeBlock value={message} hideCopyButton />
      ) : (
        <CodeBlock text={String(message)} hideCopyButton />
      )}
    </div>
  );
};
