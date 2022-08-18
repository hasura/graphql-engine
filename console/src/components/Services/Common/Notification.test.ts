import { getErrorMessage } from './Notification';

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
        expect(getErrorMessage('error message', error)).toBe('error message');
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
          expect(getErrorMessage('error message', error)).toBe(
            'foo-code: internal error'
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
          expect(getErrorMessage('error message', error)).toBe(
            'foo-code: postgres query error'
          );
        });
      });
    });

    describe('when error contains info', () => {
      it('returns the error message', () => {
        const error = {
          info: 'foo-info',
        };
        expect(getErrorMessage('error message', error)).toBe('foo-info');
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
            expect(getErrorMessage('error message', error)).toBe(
              'foo-info message error'
            );
          });
        });

        describe('when error does NOT contain message error', () => {
          it('returns the error message', () => {
            const error = {
              code: 'bar',
              message: 'bar message error',
            };
            expect(getErrorMessage('error message', error)).toBe(
              'bar message error'
            );
          });
        });

        describe('when message is a string', () => {
          it('returns the error message', () => {
            const error = {
              message: 'plain message error',
            };
            expect(getErrorMessage('error message', error)).toBe(
              'plain message error'
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
            expect(getErrorMessage('error message', error)).toBe(
              'inner-code : error message'
            );
          });
        });

        describe('when message does not contain code, message is not a string, code is not in message', () => {
          it('returns code', () => {
            const error = {};
            expect(getErrorMessage('error message', error)).toBe('');
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
        expect(getErrorMessage('error message', error)).toBe(
          `internal-error.description`
        );
      });
    });

    describe('when error contains custom', () => {
      it('returns the error message', () => {
        const error = {
          custom: 'custom-error',
        };
        expect(getErrorMessage('error message', error)).toBe(`custom-error`);
      });
    });

    describe('when error contains code, error, and path', () => {
      it('returns the error message', () => {
        const error = {
          code: 'code',
          error: 'error message',
          path: '/path',
        };
        expect(getErrorMessage('error message', error)).toBe(`error message`);
      });
    });

    describe('when error contains callToAction', () => {
      it('returns the error message', () => {
        const error = {
          callToAction: 'code',
        };
        expect(getErrorMessage('error message', error)).toBe(`error message`);
      });
    });
  });
});
