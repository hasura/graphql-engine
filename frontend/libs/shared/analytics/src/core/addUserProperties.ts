import { GlobalWindowHeap } from './heap/types';

export function addUserProperties(props: Record<string, string>) {
  (window as GlobalWindowHeap).heap?.addUserProperties(props);

  // TODO: add them to Sentry too, as tags or as breadcrumbs
}
