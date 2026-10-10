/**
 * Define a class for HTTP errors.
 */
export class HttpError<T = any> extends Error {
  constructor(
    public status: number | 'network-error',
    message: string,
    public data?: T,
  ) {
    super(message);
  }
}

export class UnauthorizedError extends Error {
  constructor(message = 'Unauthorized') {
    super(message);
  }
}

export class NotImplementedError extends Error {
  constructor(message = 'Not Implemented') {
    super(message);
  }
}
