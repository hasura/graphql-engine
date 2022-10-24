import * as React from 'react';
import { detectChildrenType } from './detectChildrenType';

function Component() {
  return <div />;
}

describe('detectChildrenType', () => {
  it.each`
    children           | description            | expectedType
    ${null}            | ${'null'}              | ${'noChildren'}
    ${undefined}       | ${'undefined'}         | ${'noChildren'}
    ${true}            | ${'a boolean'}         | ${'text'}
    ${0}               | ${'a number'}          | ${'text'}
    ${''}              | ${'a string'}          | ${'text'}
    ${(<div />)}       | ${'an HTML element'}   | ${'htmlElement'}
    ${(<></>)}         | ${'a React Fragment'}  | ${'reactComponent'}
    ${(<Component />)} | ${'a React Component'} | ${'reactComponent'}
  `(
    'should, given $description:$description, return $expectedType',
    ({ children, expectedType }) => {
      expect(detectChildrenType(children).type).toEqual(expectedType);
    }
  );
});
