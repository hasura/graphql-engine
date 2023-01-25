import { getHtmlAttributesInjectionMode } from './getHtmlAttributesInjectionMode';

describe('getHtmlAttributesInjectionMode', () => {
  it.each`
    childrenType        | passHtmlAttributesToChildren | expectedMode
    ${'text'}           | ${false}                     | ${'wrapInDiv'}
    ${'text'}           | ${true}                      | ${'wrapInDiv'}
    ${'htmlElement'}    | ${false}                     | ${'passHtmlAttributesToChildren'}
    ${'htmlElement'}    | ${true}                      | ${'passHtmlAttributesToChildren'}
    ${'reactComponent'} | ${false}                     | ${'wrapInDiv'}
    ${'reactComponent'} | ${true}                      | ${'passHtmlAttributesToChildren'}
  `(
    'should, given $childrenType:$childrenType and $passHtmlAttributesToChildren:,passHtmlAttributesToChildren return $expectedMode',
    ({ childrenType, passHtmlAttributesToChildren, expectedMode }) => {
      expect(
        getHtmlAttributesInjectionMode(
          childrenType,
          passHtmlAttributesToChildren
        )
      ).toEqual(expectedMode);
    }
  );
});
