import * as React from 'react';

export const LabelValue: React.VFC<{
  label: React.ReactNode;
  value: React.ReactNode;
}> = props => {
  const { label, value } = props;
  return (
    <div className="flex flex-col">
      <b className="text-muted">{label}: </b>
      <span className="text-muted font-normal">{value}</span>
    </div>
  );
};
