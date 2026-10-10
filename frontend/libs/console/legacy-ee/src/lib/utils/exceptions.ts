/**
 * Custom error to catch UncaughtException.
 */
export default class UncaughtException extends Error {
  constructor(requestError, error) {
    super();
    this.name = 'Uncaught Exception';
    this.message =
      'Uncaught exception: ' +
      `RequestError: ${JSON.stringify(requestError)}\n` +
      `Error: ${JSON.stringify(error)}`;
    this.stack = error.stack;
  }
}
