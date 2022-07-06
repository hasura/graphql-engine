import React from 'react';

const Input: React.FC<React.ComponentProps<'input'>> = props => (
  <input className="text-base  bg-transparent border-0 w-full" {...props} />
);

export default Input;
