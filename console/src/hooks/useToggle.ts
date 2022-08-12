import React, { useState } from 'react';

export const useToggle = (
  initialValue: boolean
): [boolean, () => void, React.Dispatch<React.SetStateAction<boolean>>] => {
  const [value, setValue] = useState(initialValue);
  const toggleValue = () => setValue(prev => !prev);

  return [value, toggleValue, setValue];
};
