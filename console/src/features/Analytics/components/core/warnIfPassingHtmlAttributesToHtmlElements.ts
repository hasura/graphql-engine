export function warnIfPassingHtmlAttributesToHtmlElements(name: string) {
  const error = new Error(
    `Passing HTML attributes is already the default behavior for DOM elements, you can remove the 'passHtmlAttributesToChildren' option passes to the element with name "${name}"`
  );
  console.warn(error);
}
