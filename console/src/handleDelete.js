export const handleDelete = (confirmMessage, Type) => {
  if (Type === true) {
    return prompt(confirmMessage);
  }
  return confirm(confirmMessage);
};
