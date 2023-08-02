import * as React from 'react';
import { Button } from '../../../../new-components/Button';
import { childrenAreKnownToAcceptHTMLAttributes } from './childrenAreKnownToAcceptHTMLAttributes';

function Component() {
  return <div />;
}

describe('childrenAreKnownToAcceptHTMLAttributes', () => {
  it.each`
    children           | description            | expectedResult
    ${null}            | ${'null'}              | ${false}
    ${undefined}       | ${'undefined'}         | ${false}
    ${true}            | ${'a boolean'}         | ${false}
    ${0}               | ${'a number'}          | ${false}
    ${''}              | ${'a string'}          | ${false}
    ${(<div />)}       | ${'an HTML element'}   | ${false}
    ${(<></>)}         | ${'a React Fragment'}  | ${false}
    ${(<Component />)} | ${'a React Component'} | ${false}
    ${(<Button />)}    | ${'<Button />'}        | ${true}
  `(
    'should, given $description, return $expectedResult',
    ({ children, expectedResult }) => {
      expect(childrenAreKnownToAcceptHTMLAttributes(children)).toEqual(
        expectedResult
      );
    }
  );
});
