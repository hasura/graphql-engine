/* Add error messages when trying to use the function */
export const getParentNodeByAttribute = (node, selector) => {
  if (node && !node.documentElement) {
    return node.hasAttribute(selector)
      ? node
      : getParentNodeByAttribute(node.parentNode, selector);
  }
  return null;
};

export const getParentNodeByClass = (node, selector) => {
  if (node && !node.documentElement) {
    return node.classList.contains(selector)
      ? node
      : getParentNodeByClass(node.parentNode, selector);
  }
  return null;
};
