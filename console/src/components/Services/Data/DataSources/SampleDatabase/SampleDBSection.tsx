import * as React from 'react';
import { SampleDBButton } from './SampleDBButton';

type Props = { onTrySampleDB: VoidFunction };

let timer: NodeJS.Timeout;

export const SampleDBSection: React.FC<Props> = ({ onTrySampleDB }) => {
  const [showSuccess, setShowSuccess] = React.useState(false);

  const onButtonClick = () => {
    if (timer) {
      clearTimeout(timer);
    }
    onTrySampleDB();
    setShowSuccess(true);
    timer = setTimeout(() => {
      setShowSuccess(false);
    }, 5000);
  };

  React.useEffect(() => {
    return () => {
      if (timer) {
        clearTimeout(timer);
      }
    };
  }, []);

  return (
    <div className="flex flex-col w-full items-start">
      <div className="w-full flex justify-start items-center">
        <p className="m-0 mr-sm">Don&apos;t have a database?</p>
        <SampleDBButton onClick={onButtonClick} />
      </div>
      {showSuccess && (
        <i className="mt-xs text-green-800 text-sm">
          The Database credentials have been populated in the form below. Please
          click &apos;Connect Database&apos; button to continue.
        </i>
      )}
    </div>
  );
};
