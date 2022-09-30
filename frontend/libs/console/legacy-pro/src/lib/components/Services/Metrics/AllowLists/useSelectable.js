import { useState } from 'react';

const defaultState = {
  items: [],
};

const useSelectable = () => {
  const [data, update] = useState(defaultState);
  const { items } = data;
  const modifyState = item => {
    if (typeof item === 'object') {
      update(s => {
        return {
          ...s,
          items: [...item],
        };
      });
      return;
    }
    const elementPos = items.indexOf(item);
    if (elementPos !== -1) {
      update(s => {
        return {
          ...s,
          items: [
            ...s.items.slice(0, elementPos),
            ...s.items.slice(elementPos + 1),
          ],
        };
      });
      return;
    }
    update(s => {
      return {
        ...s,
        items: [...s.items, item],
      };
    });
  };

  return [data.items, modifyState];
};

export default useSelectable;
