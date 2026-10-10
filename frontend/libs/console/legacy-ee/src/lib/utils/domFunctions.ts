export const getParentNodeByClass = (node, selector) => {
  if (node && !node.documentElement) {
    return node.classList.contains(selector)
      ? node
      : getParentNodeByClass(node.parentNode, selector);
  }
  return null;
};
