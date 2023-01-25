import { onClick } from './onClick';

describe('onClick', () => {
  // @see: https://sentry.io/organizations/hasura-nn/issues/3679294754/activity/?project=6684052
  // @see: https://sentry.io/organizations/hasura-nn/issues/3679762841/activity/?project=6684052
  describe('When invoked with a non-valid event then should not throw (Sentry CONSOLE-6J and CONSOLE-6E)', () => {
    it.each`
      testCase                        | event
      ${'null'}                       | ${null}
      ${'empty object'}               | ${{}}
      ${'null target'}                | ${{ target: null }}
      ${'null focus'}                 | ${{ target: { focus: null } }}
      ${'null closest'}               | ${{ target: { focus: null, closest: null } }}
      ${'closest returns null'}       | ${{ target: { focus: null, closest: () => null } }}
      ${'null querySelector'}         | ${{ target: { focus: null, closest: () => ({ querySelector: null }) } }}
      ${'querySelector returns null'} | ${{ target: { focus: null, closest: () => ({ querySelector: () => null }) } }}
    `(`Test case: '$testCase'`, ({ event }) => {
      expect(() => onClick(event)).not.toThrow();
    });
  });

  it(`When invoked with a valid event, then should call the target' focus method`, () => {
    // Arrange
    const target = document.createElement('p');
    target.focus = jest.fn();
    const fakeEvent = { target };

    // Act
    onClick(fakeEvent);

    // Assert
    expect(target.focus).toHaveBeenCalled();
  });

  it(`When invoked with a valid event and a .radio-inline element exist, then should set the element's checked property`, () => {
    // Arrange
    const target = document.createElement('input');
    const fakeEvent = { target };

    // simulating a "real" DOM case where the closes element exists
    const fakeElement = { checked: false };
    target.closest = () => ({ querySelector: () => fakeElement });

    // Act
    onClick(fakeEvent);

    // Assert
    expect(fakeElement.checked).toBe(true);
  });
});
