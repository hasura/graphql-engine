/**
 * A heap object that attempts to mirror the actual heap API, while handling the nullability check.
 */
export const heap = {
  addUserProperties: (props: Record<string, string>) => {
    window.heap?.addUserProperties(props);
  },
};
