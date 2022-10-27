export function warnIfTitleElementIsPassed(name: string) {
  const error = new Error(
    `title tags cannot be wrapped by <Analytics /> because it will not work, even if used through Helmet (name: "${name}")`
  );
  console.error(error);
}
