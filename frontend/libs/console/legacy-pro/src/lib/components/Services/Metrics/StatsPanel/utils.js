const nextGroupValue = (prev, val) => {
  const i = prev.indexOf(val);
  if (i !== -1) {
    return [...prev.slice(0, i), ...prev.slice(i + 1)];
  }
  return [...prev, val];
};

const indexOf = (list, predicateFn) => {
  let elementIndex = -1;
  list.forEach((l, index) => {
    if (predicateFn(l)) {
      elementIndex = index;
    }
  });
  return elementIndex;
};

const nextFilterValue = (prev, type, value, isSingleSelect) => {
  const predicateFn = l => {
    return l.value === value && l.type === type;
  };
  const elementIndex = indexOf(prev, predicateFn);
  if (elementIndex !== -1) {
    return [...prev.slice(0, elementIndex), ...prev.slice(elementIndex + 1)];
  }
  if (isSingleSelect) {
    const newTimeRangeFilter = [...prev.filter(f => f.type !== type)];
    return [
      ...newTimeRangeFilter,
      {
        value: value,
        type: type,
      },
    ];
  }
  return [
    ...prev,
    {
      type: type,
      value: value,
    },
  ];
};

export { nextGroupValue, nextFilterValue };
