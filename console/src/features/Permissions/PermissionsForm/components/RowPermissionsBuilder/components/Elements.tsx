import React from 'react';
import clsx from 'clsx';

interface Props extends React.ComponentProps<'div'> {
  text: JsonOptions;
}

type JsonOptions = '{' | '}' | '[' | ']' | '},' | ',' | '"' | ':';

const chooseColor = (text: JsonOptions) => {
  switch (text) {
    case '{':
    case '}':
    case '},':
      return 'text-blue-800';
    case '[':
    case ']':
      return 'text-yellow-500';
    default:
      return 'text-black';
  }
};

export const JsonItem = (props: Props) => {
  const color = chooseColor(props.text);

  return (
    <span className={clsx('font-bold text-lg', color, props.className)}>
      {props.text}
    </span>
  );
};
