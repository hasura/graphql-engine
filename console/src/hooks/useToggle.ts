import { useState } from 'react';

export const useToggle = (initialValue: boolean): [boolean, () => void] => {
  const [value, setValue] = useState(initialValue);
  const toggleValue = () => setValue(prev => !prev);

  return [value, toggleValue];
};
