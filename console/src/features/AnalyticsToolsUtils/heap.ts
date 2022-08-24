/**
 * A heap object that attempts to mirror the actual heap API, while handling the nullability check
 * Currently only implements `addUserProperties`. More functions: identify, track etc can be added
 */
export const heap = {
  addHeapUserProperties: (props: Record<string, string>) => {
    window.heap?.addUserProperties(props);
  },
};
