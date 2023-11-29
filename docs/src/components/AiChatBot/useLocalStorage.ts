import { useState } from 'react';
export const useLocalStorage = <T>(key: string, defaultValue: T) => {
  // Create state variable to store localStorage value in state
  const [localStorageValue, setLocalStorageValue] = useState(() => {
    try {
      const value = localStorage.getItem(key);
      // If value is already present in localStorage then return it

      // Else set default value in localStorage and then return it
      if (value) {
        let parsedValue = JSON.parse(value);

        if (Array.isArray(parsedValue)) {
          const filteredValue = parsedValue.filter(item => !!item);

          // Update localStorage if non-truthy values were filtered out
          if (filteredValue.length !== parsedValue.length) {
            parsedValue = filteredValue;
            localStorage.setItem(key, JSON.stringify(filteredValue));
          }
        }

        return parsedValue as T;
      } else {
        localStorage.setItem(key, JSON.stringify(defaultValue));
        return defaultValue;
      }
    } catch (error) {
      localStorage.setItem(key, JSON.stringify(defaultValue));
      return defaultValue;
    }
  });

  // this method update our localStorage and our state
  const setLocalStorageStateValue = valueOrFn => {
    let newValue: T;
    if (typeof valueOrFn === 'function') {
      const fn = valueOrFn as (value: T) => T;
      newValue = fn(localStorageValue);
    } else {
      newValue = valueOrFn;
    }

    // Filter out non-truthy values if newValue is an array
    if (Array.isArray(newValue)) {
      newValue = newValue.filter(item => !!item) as T;
    }

    localStorage.setItem(key, JSON.stringify(newValue));
    setLocalStorageValue(newValue);
  };

  return [localStorageValue, setLocalStorageStateValue] as const;
};

export default useLocalStorage;
