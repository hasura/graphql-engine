import * as React from 'react';

type Props = {
  onClick: VoidFunction;
};

export const SampleDBButton: React.FC<Props> = ({ onClick }) => {
  return (
    <button
      type="button"
      className="bg-cloud p-sm rounded text-white font-semibold shadow hover:shadow-lg"
      onClick={e => {
        e.preventDefault();
        onClick();
      }}
    >
      Use a sample read-only database
    </button>
  );
};
