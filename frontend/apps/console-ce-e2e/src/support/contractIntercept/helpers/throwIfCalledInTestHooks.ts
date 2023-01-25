/**
 * Throws if called from inside a test hook.
 * The problem is that it's impossible to know which test the hook relates to. Hence, it's
 * impossible to detect the name of the test.
 */
export function throwIfCalledInTestHooks(testTitle: string) {
  switch (testTitle) {
    case '"after" hook':
      throw new Error(
        'interceptAndRecordContract cannot be called inside a "after" hook'
      );
    case '"after all" hook':
      throw new Error(
        'interceptAndRecordContract cannot be called inside a "after all" hook'
      );
    case '"before" hook':
      throw new Error(
        'interceptAndRecordContract cannot be called inside a "before" hook'
      );
    case '"before all" hook':
      throw new Error(
        'interceptAndRecordContract cannot be called inside a "before all" hook'
      );
  }
}
