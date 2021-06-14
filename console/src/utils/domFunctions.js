/* Add error messages when trying to use the function */
export const getParentNodeByAttribute = (node, selector) => {
  if (node && !node.documentElement) {
    return node.hasAttribute(selector)
      ? node
      : getParentNodeByAttribute(node.parentNode, selector);
  }
  return null;
};
