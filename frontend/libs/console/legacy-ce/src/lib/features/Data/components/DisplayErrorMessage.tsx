import AceEditor from 'react-ace';

export const DisplayToastErrorMessage = ({ message }: { message: string }) => {
  return (
    <div className="overflow-hidden py-1.5">
      <AceEditor
        theme="github"
        setOptions={{
          minLines: 1,
          wrap: true,
          useWrapMode: true,
          indentedSoftWrap: true,
          maxLines: 50,
          showGutter: false,
          useWorker: false,
        }}
        value={message}
      />
    </div>
  );
};
