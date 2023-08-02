import React from 'react';

type FieldLabelProps = {
  title: string;
};

export const FieldLabel = ({ title }: FieldLabelProps) => {
  return (
    <div className="text-xs pt-2 uppercase text-gray-400 tracking-wide font-semibold">
      {title}
    </div>
  );
};
