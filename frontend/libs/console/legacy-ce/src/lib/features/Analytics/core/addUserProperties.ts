export function addUserProperties(props: Record<string, string>) {
  window.heap?.addUserProperties(props);

  // TODO: add them to Sentry too, as tags or as breadcrumbs
}
