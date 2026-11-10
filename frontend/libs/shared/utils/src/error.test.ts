import { HttpError } from '@hasura/shared/types';
import {
  formatDependentObjectsError,
  getDependencyErrors,
  getDependentObjects,
  getErrorContent,
  getErrorMessage,
} from './error';

const dependentObjectsMessage =
  'cannot drop due to the following dependent objects: relationship ArticlesToArea.area in source "default", relationship Company.area in source "default"';

describe('formatDependentObjectsError', () => {
  it('puts each dependent object on its own line', () => {
    expect(formatDependentObjectsError(dependentObjectsMessage)).toBe(
      [
        'cannot drop due to the following dependent objects:',
        '- relationship ArticlesToArea.area in source "default"',
        '- relationship Company.area in source "default"',
      ].join('\n'),
    );
  });

  it('keeps names that contain a comma in one object', () => {
    expect(
      getDependentObjects(
        new Error(
          'cannot drop due to the following dependent objects: function search_albums in source "db, eu", relationship Artist.Albums in source "db, eu", remote_relationship album on type Film in remote schema test',
        ),
      ),
    ).toEqual([
      'function search_albums in source "db, eu"',
      'relationship Artist.Albums in source "db, eu"',
      'remote_relationship album on type Film in remote schema test',
    ]);
  });

  it('ignores other messages', () => {
    expect(formatDependentObjectsError('field not found')).toBeNull();
    expect(formatDependentObjectsError(undefined)).toBeNull();
  });

  it('lists the dependent objects of a dependency error', () => {
    const error = new HttpError(400, 'Bad Request', {
      code: 'dependency-error',
      error: dependentObjectsMessage,
      path: '$.args',
    });
    expect(getDependentObjects(error)).toEqual([
      'relationship ArticlesToArea.area in source "default"',
      'relationship Company.area in source "default"',
    ]);
    expect(
      getDependentObjects(new Error(dependentObjectsMessage)),
    ).toHaveLength(2);
    expect(
      getDependentObjects(new HttpError(400, 'Bad Request', { error: 'x' })),
    ).toBeNull();
    expect(getDependentObjects(undefined)).toBeNull();
  });

  it('lists the dependent objects of each table of a bulk untrack', () => {
    const error = new HttpError(400, 'Bad Request', {
      code: 'invalid-configuration',
      error: 'all tables failed to untrack',
      path: '$.args',
      internal: [
        {
          code: 'untrack-table-failed',
          message: dependentObjectsMessage,
          name: 'table Album in source default',
          type: 'table',
        },
        {
          code: 'untrack-table-failed',
          message: 'some other failure',
          name: 'table Track in source default',
          type: 'table',
        },
      ],
    });
    expect(getDependencyErrors(error)).toEqual([
      {
        name: 'table Album in source default',
        objects: [
          'relationship ArticlesToArea.area in source "default"',
          'relationship Company.area in source "default"',
        ],
        message: dependentObjectsMessage,
      },
      {
        name: 'table Track in source default',
        objects: null,
        message: 'some other failure',
      },
    ]);
  });

  it('reads the warnings of a partially successful bulk untrack', () => {
    expect(
      getDependencyErrors({
        message: 'success',
        warnings: [
          {
            code: 'untrack-table-failed',
            message: dependentObjectsMessage,
            name: 'table Album in source default',
            type: 'table',
          },
        ],
      }),
    ).toEqual([
      {
        name: 'table Album in source default',
        objects: [
          'relationship ArticlesToArea.area in source "default"',
          'relationship Company.area in source "default"',
        ],
        message: dependentObjectsMessage,
      },
    ]);
    expect(getDependencyErrors({ message: 'success' })).toBeNull();
  });

  it('ignores bulk failures without dependent objects', () => {
    const error = new HttpError(400, 'Bad Request', {
      code: 'invalid-configuration',
      error: 'all tables failed to untrack',
      internal: [{ code: 'untrack-table-failed', message: 'nope' }],
    });
    expect(getDependencyErrors(error)).toBeNull();
    expect(getDependencyErrors(new Error('nope'))).toBeNull();
  });

  it('is used for the error content of a failed metadata call', () => {
    const error = new HttpError(400, 'Bad Request', {
      code: 'dependency-error',
      error: dependentObjectsMessage,
      path: '$.args',
    });
    expect(getErrorContent(error)).toBe(
      formatDependentObjectsError(dependentObjectsMessage),
    );
  });
});

describe('getErrorMessage', () => {
  describe('when error is undefined', () => {
    it('returns the error message', () => {
      expect(getErrorMessage('error message')).toBe('error message');
    });
  });

  describe('when error is defined', () => {
    describe('when error has callToAction reload-metadata', () => {
      it('returns the error message', () => {
        const error: Record<string, any> = {
          callToAction: 'reload-metadata',
          callback: () => null,
        };
        expect(getErrorMessage(error, 'error message')).toBe('error message');
      });
    });
  });

  describe('when error is string', () => {
    it('returns the error as message', () => {
      const error = 'error message';
      expect(getErrorMessage('error message', error)).toBe('error message');
    });
  });

  describe('when error is a record', () => {
    describe('when message error is "postgres query error"', () => {
      describe('when message internal is defined', () => {
        it('returns the error message', () => {
          const error = {
            message: {
              code: 'foo-code',
              error: 'postgres query error',
              internal: {
                error: {
                  message: 'internal error',
                },
              },
            },
          };
          expect(getErrorMessage(error, 'error message')).toBe(
            'foo-code: internal error',
          );
        });
      });

      describe('when message internal is NOT defined', () => {
        it('returns the error message', () => {
          const error = {
            code: 'foo-code',
            message: {
              error: 'postgres query error',
            },
          };
          expect(getErrorMessage(error, 'error message')).toBe(
            'foo-code: postgres query error',
          );
        });
      });
    });

    describe('when error contains info', () => {
      it('returns the error message', () => {
        const error = {
          info: 'foo-info',
        };
        expect(getErrorMessage(error, 'error message')).toBe('foo-info');
      });
    });

    describe('when error contains message', () => {
      describe('when error contains code', () => {
        describe('when error contains message error', () => {
          it('returns the error message', () => {
            const error = {
              code: 'foo-info',
              message: {
                error: {
                  message: 'foo-info message error',
                },
              },
            };
            expect(getErrorMessage(error, 'error message')).toBe(
              'foo-info message error',
            );
          });
        });

        describe('when error does NOT contain message error', () => {
          it('returns the error message', () => {
            const error = {
              code: 'bar',
              message: 'bar message error',
            };
            expect(getErrorMessage(error, 'error message')).toBe(
              'bar message error',
            );
          });
        });

        describe('when message is a string', () => {
          it('returns the error message', () => {
            const error = {
              message: 'plain message error',
            };
            expect(getErrorMessage(error, 'error message')).toBe(
              'plain message error',
            );
          });
        });

        describe('when message contains code', () => {
          it('returns the error message', () => {
            const error = {
              message: {
                code: 'inner-code',
              },
            };
            expect(getErrorMessage(error, 'error message')).toBe(
              'inner-code : error message',
            );
          });
        });

        describe('when message does not contain code, message is not a string, code is not in message', () => {
          it('returns the fallback message', () => {
            const error = {};
            expect(getErrorMessage(error, 'error message')).toBe(
              'error message',
            );
          });
        });
      });
    });

    describe('when error contains internal and error', () => {
      it('returns the error message', () => {
        const error = {
          internal: {
            error: {
              message: 'internal-error',
              description: 'description',
            },
          },
        };
        expect(getErrorMessage(error, 'error message')).toBe(
          `internal-error.description`,
        );
      });
    });

    describe('when error contains custom', () => {
      it('returns the error message', () => {
        const error = {
          custom: 'custom-error',
        };
        expect(getErrorMessage(error, 'error message')).toBe(`custom-error`);
      });
    });

    describe('when error contains code, error, and path', () => {
      it('returns the error message', () => {
        const error = {
          code: 'code',
          error: 'error message',
          path: '/path',
        };
        expect(getErrorMessage(error, 'error message')).toBe(`error message`);
      });
    });

    describe('when error contains callToAction', () => {
      it('returns the error message', () => {
        const error = {
          callToAction: 'code',
        };
        expect(getErrorMessage(error, 'error message')).toBe(`error message`);
      });
    });
  });
});
