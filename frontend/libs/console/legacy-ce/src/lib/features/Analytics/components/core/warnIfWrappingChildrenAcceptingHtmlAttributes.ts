export function warnIfWrappingChildrenAcceptingHtmlAttributes(name: string) {
  const error = new Error(
    `The children accept HTML attributes but you are not using the 'passHtmlAttributesToChildren' prop to the element with name "${name}"`
  );
  console.error(error);
}
