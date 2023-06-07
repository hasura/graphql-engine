namespace APIErrors {
  export interface PgError {
    message: {
      error: 'postgres query error' | 'query execution failed';
      internal?: {
        error?: {
          message: string;
        };
      };
      code: string;
    };
    code: string;
  }

  export interface InternalError {
    internal: {
      error: {
        message: string;
        description: string;
      };
    };
  }

  export type GeneralError = {
    code?: string;
    message:
      | string
      | {
          error?: {
            message: string;
          };
          code?: string;
        };
  };

  export const hasField = <T extends string, U = unknown>(
    e: unknown,
    check: T
  ): e is Record<T, U> => {
    if (typeof e !== 'object' || e === null) return false;
    return check in e;
  };

  export const hasStringField = <T extends string>(
    e: unknown,
    check: T
  ): e is Record<T, string> => {
    return hasField<T, string>(e, check);
  };

  export const isPgError = (e: unknown): e is PgError => {
    if (!hasField(e, 'message')) return false;
    if (!hasField(e.message, 'error')) return false;
    const errString = e.message.error;
    return (
      errString === 'postgres query error' ||
      errString === 'query execution failed'
    );
  };

  export const isInternalError = (e: unknown): e is InternalError => {
    return hasField(e, 'error') && hasField(e.error, 'internal');
  };

  export const isGeneralError = (e: unknown): e is GeneralError => {
    return hasField(e, 'message');
  };
}

export class APIError extends Error {
  public override name = 'APIError';
  public override message: string;
  public code?: string;
  public description?: string;
  constructor(message: string, code?: string, description?: string) {
    super(message);
    this.message = message;
    this.code = code;
    this.description = description;
  }

  // TODO: write tests for this
  static fromUnknown(error: unknown): APIError {
    const defaultMessage = 'request failed';
    if (typeof error === 'string') return new APIError(error);
    if (APIErrors.isPgError(error)) {
      if (error.message.internal) {
        const message = error.message.internal.error?.message;
        return new APIError(message ?? defaultMessage, error.message.code);
      }
      return new APIError(error.message.error, error.code);
    }
    if (APIErrors.hasStringField(error, 'info')) {
      return new APIError(error.info);
    }
    if (APIErrors.isGeneralError(error)) {
      if (error.code) {
        if (typeof error.message === 'string') {
          return new APIError(error.message, error.code);
        }
        if (error.message.error) {
          return new APIError(error.message.error.message, error.code);
        }
      }
      if (typeof error.message === 'string') {
        return new APIError(error.message);
      }
      if (APIErrors.hasStringField(error.message, 'code')) {
        return new APIError(defaultMessage, error.message.code);
      }
    }
    if (APIErrors.isInternalError(error)) {
      const e = error.internal.error;
      return new APIError(e.message, undefined, e.description);
    }
    if (APIErrors.hasStringField(error, 'custom')) {
      return new APIError(error.custom);
    }
    if (
      APIErrors.hasStringField(error, 'code') &&
      APIErrors.hasStringField(error, 'error') &&
      APIErrors.hasStringField(error, 'path')
    ) {
      return new APIError(error.error, error.code);
    }
    return new APIError(JSON.stringify(error ?? defaultMessage));
  }
}
