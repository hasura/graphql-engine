/* Add error messages when trying to use the function */
export const getMeParentNode = (node, selector) => {
  if (node && !node.documentElement) {
    return node.hasAttribute(selector)
      ? node
      : getMeParentNode(node.parentNode, selector);
  }
  return null;
};

export const getMeParentNodeByClass = (node, selector) => {
  if (node && !node.documentElement) {
    return node.classList.contains(selector)
      ? node
      : getMeParentNodeByClass(node.parentNode, selector);
  }
  return null;
};
