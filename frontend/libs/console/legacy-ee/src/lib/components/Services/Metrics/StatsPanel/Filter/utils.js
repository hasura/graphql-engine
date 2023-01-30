const compose = sourceFn => type => value => {
  sourceFn(type, value);
};
const splitByType = (filters, type) => {
  const allNonDropdownFilters = filters.filter(f => f.type !== type);
  const allDropdownFilters = filters.filter(f => f.type === type);

  return [allDropdownFilters, allNonDropdownFilters];
};

const detectAdjective = n => {
  if (n > 1) return 's';
  return '';
};

const getSelectedFiltersCount = selectedValues => {
  let filteredElement = 'No filter applied';

  if (selectedValues.length > 0) {
    filteredElement = `${selectedValues.length} filter${detectAdjective(
      selectedValues.length
    )} applied`;
  }
  return filteredElement;
};

const filterByType = (filters, type) => filters.filter(f => f.type === type);

/* Converts an array of json with the following structure [{
 *   type: "operation_type",
 *   value: "getAuthors",
 * }] -> {
 *  getAuthors: true
 * }
 * */
const getJson = (list, key) => {
  const r = {};
  list.forEach(l => {
    if (typeof l[key] === 'string') {
      r[l[key]] = true;
    } else {
      r.Custom = true;
    }
  });
  return r;
};

export { splitByType, getSelectedFiltersCount, filterByType, getJson, compose };
