import { errorMustBeBlocked } from './errorMustBeBlocked';

describe('errorMustBeBlocked', () => {
  test('When the error is null, then should return false', () => {
    expect(
      errorMustBeBlocked({
        error: null,
        urlPrefix: '/console',
        pathname: '/console',
      })
    ).toEqual(false);
  });

  test('When the error is en empty string, then should return false', () => {
    expect(
      errorMustBeBlocked({
        error: '',
        urlPrefix: '/console',
        pathname: '/console',
      })
    ).toEqual(false);
  });

  test('When the error is a string, then should be threated as an error', () => {
    const errorToBlock =
      '"__twoDashesColumn" must not begin with "__", which is reserved by GraphQL introspection';
    expect(
      errorMustBeBlocked({
        error: errorToBlock,
        urlPrefix: '/console',
        pathname: '/console',
      })
    ).toEqual(true);
  });

  test('When the path name is /, then should always return false', () => {
    const errorToBlock =
      '"__twoDashesColumn" must not begin with "__", which is reserved by GraphQL introspection';
    expect(
      errorMustBeBlocked({
        error: errorToBlock,
        urlPrefix: '/console',
        pathname: '/',
      })
    ).toEqual(false);
  });

  test.each`
    urlPrefix                 | pathname
    ${'/console'}             | ${'/console'}
    ${'/console'}             | ${'/console/'}
    ${'/console'}             | ${'/api/api-explorer'}
    ${'/console'}             | ${'/api/api-explorer/'}
    ${'/console'}             | ${'/console/api/api-explorer'}
    ${'/customConsolePrefix'} | ${'/customConsolePrefix'}
    ${'/customConsolePrefix'} | ${'/customConsolePrefix/'}
    ${'/customConsolePrefix'} | ${'/api/api-explorer'}
    ${'/customConsolePrefix'} | ${'/api/api-explorer/'}
    ${'/customConsolePrefix'} | ${'/customConsolePrefix/api/api-explorer'}
  `(
    `When the urlPrefix is '$urlPrefix' and the pathname is '$pathname', then should return true`,
    ({ urlPrefix, pathname }) => {
      const errorToBlock = new Error(
        '"__twoDashesColumn" must not begin with "__", which is reserved by GraphQL introspection'
      );

      expect(
        errorMustBeBlocked({
          error: errorToBlock,
          urlPrefix,
          pathname,
        })
      ).toEqual(true);
    }
  );

  test.each`
    error
    ${new Error('"__twoDashesColumn" must not begin with "__", which is reserved by GraphQL introspection')}
    ${new Error('Input Object type msgbox_UserInbox_stream_cursor_value_input must define one or more fields.')}
    ${new Error(`Cannot read properties of undefined (reading 'variableDefinitions')`)}
    ${new Error(`Cannot read properties of undefined (reading 'getFields')`)}
  `(`When the error is '$error', then should return true`, ({ error }) => {
    expect(
      errorMustBeBlocked({
        error,
        urlPrefix: '/console',
        pathname: '/console',
      })
    ).toEqual(true);
  });
});
