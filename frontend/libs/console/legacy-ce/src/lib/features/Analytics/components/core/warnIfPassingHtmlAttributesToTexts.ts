export function warnIfPassingHtmlAttributesToTexts(name: string) {
  const error = new Error(
    `Passing HTML attributes to strings is not does not make sense, you must remove the 'passHtmlAttributesToChildren' option passes to the element with name "${name}"`
  );
  console.error(error);
}
