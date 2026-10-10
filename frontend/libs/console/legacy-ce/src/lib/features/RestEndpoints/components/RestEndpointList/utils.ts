import { AllowedRestMethods } from '@hasura/shared/types';

const positionsMap = {
  GET: 1,
  POST: 2,
  PUT: 3,
  PATCH: 4,
  DELETE: 5,
};

const modifyMethodsList = (methods: AllowedRestMethods[]) =>
  methods.map((method) => ({ name: method, position: positionsMap[method] }));

export const badgeSort = (methods: AllowedRestMethods[]) => {
  const modifiedMethods = modifyMethodsList(methods);
  return modifiedMethods
    .sort((a, b) => a.position - b.position)
    .map((method) => method.name);
};
