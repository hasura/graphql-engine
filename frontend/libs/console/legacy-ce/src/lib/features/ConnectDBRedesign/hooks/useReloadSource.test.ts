import type { InconsistentObject } from '@hasura/shared/types';
import { isSourceInconsistentAfterReload } from './useReloadSource';

const unreachableSource = {
  definition: 'chinook',
  name: 'source chinook',
  reason: 'Inconsistent object: could not connect to server',
  type: 'source',
} as InconsistentObject;

describe('isSourceInconsistentAfterReload', () => {
  it('is true when the reloaded source is reported as inconsistent', () => {
    expect(
      isSourceInconsistentAfterReload(
        { is_consistent: false, inconsistent_objects: [unreachableSource] },
        'chinook',
      ),
    ).toBe(true);
  });

  it('is false when only other sources are inconsistent', () => {
    expect(
      isSourceInconsistentAfterReload(
        { is_consistent: false, inconsistent_objects: [unreachableSource] },
        'default',
      ),
    ).toBe(false);
  });

  it('is false when the metadata is consistent', () => {
    expect(
      isSourceInconsistentAfterReload({ is_consistent: true }, 'chinook'),
    ).toBe(false);
  });
});
