import React from 'react';

const Input: React.FC<React.ComponentProps<'input'>> = (props) => (
  <input className="bg-transparent border-0 w-full outline-0" {...props} />
);

export default Input;
