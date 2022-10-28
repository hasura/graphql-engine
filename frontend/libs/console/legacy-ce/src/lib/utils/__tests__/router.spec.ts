import { composeOnEnterHooks } from '../router';

describe('composeOnEnterHooks', () => {
  it('returns an onEnterHook function that calls the final callback after each other cb has been called', () => {
    const hook0 = jest.fn();
    const hook1 = jest.fn();
    const cb = jest.fn();
    const composed = composeOnEnterHooks([hook0, hook1]);
    composed({ location: {} }, () => {}, cb);
    expect(hook0).toBeCalledTimes(1);
    expect(hook1).toBeCalledTimes(1);
    expect(cb).toBeCalledTimes(1);
  });
  it('works with array of any length', () => {
    const hook0 = jest.fn();
    const hook1 = jest.fn();
    const hook2 = jest.fn();
    const cb = jest.fn();
    composeOnEnterHooks([hook0, hook0, hook0])({ location: {} }, () => {}, cb);
    composeOnEnterHooks([hook1, hook1])({ location: {} }, () => {}, cb);
    composeOnEnterHooks([hook2])({ location: {} }, () => {}, cb);
    expect(hook0).toBeCalledTimes(3);
    expect(hook1).toBeCalledTimes(2);
    expect(hook2).toBeCalledTimes(1);
    expect(cb).toBeCalledTimes(3);
  });
});
