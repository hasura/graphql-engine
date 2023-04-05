import React from 'react';
import { CgSpinner } from 'react-icons/cg';

type Props = {
  message: string;
  showIcon?: boolean;
};

export function LoadingMessage(props: Props) {
  const { message, showIcon = true } = props;
  return (
    <div className="text-muted flex items-center text-sm mt-1">
      {showIcon ? <CgSpinner className="animate-spin w-4 h-4 mr-1" /> : null}
      {message}
    </div>
  );
}
