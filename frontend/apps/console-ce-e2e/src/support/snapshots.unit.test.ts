/**
 * No-backend unit test for the snapshot helper's pure logic. Runs inside Cypress
 * via the `src/support/**\/*unit.test.{js,ts}` specPattern — no HGE/console needed.
 *
 * The guard behaviours (null subject FAILS, missing snapshot FAILS, never
 * auto-create-and-pass) are extracted into pure functions so they can be asserted
 * directly, instead of relying on `cy.on('fail')` (which can silently swallow an
 * unexpected assertion error). One integration test still proves the command is
 * actually wired to the guard, using a safe `cy.on('fail')` pattern that forwards
 * unexpected errors to `done` rather than hiding them.
 */
import {
  normalize,
  snapshotsEqual,
  assertSnapshotSubjectProduced,
  decideSnapshotOutcome,
} from './snapshots';

describe('snapshot helper logic', () => {
  it('normalize sorts object keys deeply and preserves arrays/primitives', () => {
    const input = { b: 1, a: { d: 2, c: [{ y: 1, x: 2 }] } };
    expect(JSON.stringify(normalize(input))).to.equal(
      JSON.stringify({ a: { c: [{ x: 2, y: 1 }], d: 2 }, b: 1 }),
    );
    expect(normalize(5)).to.equal(5);
    expect(JSON.stringify(normalize([3, 1, 2]))).to.equal(
      JSON.stringify([3, 1, 2]),
    );
  });

  it('snapshotsEqual is key-order insensitive (fixes stale-stored false mismatch)', () => {
    // stored (unsorted) vs actual (sorted) must still be equal
    expect(snapshotsEqual({ b: 1, a: 2 }, { a: 2, b: 1 })).to.equal(true);
    expect(snapshotsEqual({ a: 1 }, { a: 2 })).to.equal(false);
  });

  // ---- Pure guard: a null/undefined subject must throw (no silent pass) ----
  it('assertSnapshotSubjectProduced throws on null/undefined, passes otherwise', () => {
    expect(() => assertSnapshotSubjectProduced(null, 'test > a')).to.throw(
      'A snapshot of nothing is a failure',
    );
    expect(() => assertSnapshotSubjectProduced(undefined, 'test > a')).to.throw(
      'A snapshot of nothing is a failure',
    );
    // Empty arrays/objects and falsy primitives are valid, snapshot-able values.
    expect(() => assertSnapshotSubjectProduced([], 'test > a')).to.not.throw();
    expect(() => assertSnapshotSubjectProduced({}, 'test > a')).to.not.throw();
    expect(() => assertSnapshotSubjectProduced(0, 'test > a')).to.not.throw();
    expect(() => assertSnapshotSubjectProduced('', 'test > a')).to.not.throw();
  });

  // ---- Pure decision: missing snapshot FAILS, never auto-create-and-pass ----
  it('decideSnapshotOutcome never auto-creates on a normal run', () => {
    // equal -> no-op
    expect(
      decideSnapshotOutcome({
        exists: true,
        matches: true,
        updateSnapshots: false,
      }),
    ).to.equal('match');
    // missing + no opt-in -> FAIL (not a silent write-and-pass)
    expect(
      decideSnapshotOutcome({
        exists: false,
        matches: false,
        updateSnapshots: false,
      }),
    ).to.equal('missing');
    // differs + no opt-in -> FAIL with diff
    expect(
      decideSnapshotOutcome({
        exists: true,
        matches: false,
        updateSnapshots: false,
      }),
    ).to.equal('diff');
    // explicit opt-in -> write (whether creating or updating)
    expect(
      decideSnapshotOutcome({
        exists: false,
        matches: false,
        updateSnapshots: true,
      }),
    ).to.equal('write');
    expect(
      decideSnapshotOutcome({
        exists: true,
        matches: false,
        updateSnapshots: true,
      }),
    ).to.equal('write');
  });

  // ---- Integration: the command is actually wired to the null-subject guard ----
  it('toMatchSnapshot command FAILS on a null subject (end-to-end wiring)', (done) => {
    const onFail = (err: Error) => {
      cy.off('fail', onFail);
      try {
        expect(err.message).to.contain('A snapshot of nothing is a failure');
        done();
      } catch (assertionError) {
        // Forward an UNEXPECTED failure to done instead of swallowing it.
        done(assertionError);
      }
      // Returning false tells Cypress not to additionally fail the test with the
      // (expected) error we just asserted on.
      return false;
    };
    cy.on('fail', onFail);
    cy.wrap(null).toMatchSnapshot();
  });
});
