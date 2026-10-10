// `cy.wrap(value).toMatchSnapshot()` - JSON snapshot testing for e2e specs.
//
// - Snapshots are keyed by the full test title plus a per-test counter
//   (`<describe> > <it> #0`, `#1`, ...), and stored by the readSnapshot and
//   writeSnapshot tasks (see support/tasks/snapshots.ts).
// - Object keys are sorted (normalized) on BOTH the stored and the actual value
//   before comparing, so key order is irrelevant even for older stored files.
// - A null/undefined subject FAILS: a snapshot of "nothing" (e.g. a metadata
//   lookup that found nothing) means the thing under test was not produced, so
//   it must not silently pass.
// - A MISSING snapshot FAILS (it is not auto-created on a normal run). Create or
//   update snapshots deliberately with `--env updateSnapshots=true`, then commit
//   the generated files.
// - The optional `name` is only used as the command log label.

type SnapshotOptions = { name?: string };

let snapshotCounters: Record<string, number> = {};

// Reset per test (and per retry attempt), so a retried test reuses its snapshot titles.
beforeEach(() => {
  snapshotCounters = {};
});

export function normalize(value: unknown): unknown {
  if (Array.isArray(value)) {
    return value.map(normalize);
  }
  if (typeof value === 'object' && value !== null) {
    return Object.keys(value)
      .sort()
      .reduce<Record<string, unknown>>((result, key) => {
        result[key] = normalize((value as Record<string, unknown>)[key]);
        return result;
      }, {});
  }
  return value;
}

/** Deep, key-order-insensitive equality for two already-JSON-safe values. */
export function snapshotsEqual(expected: unknown, actual: unknown): boolean {
  return (
    JSON.stringify(normalize(expected)) === JSON.stringify(normalize(actual))
  );
}

/**
 * Pure guard: a null/undefined subject is a failure, not a silent pass — it means
 * the value under test was never produced (e.g. a metadata lookup found nothing).
 * Throws the exact error the command surfaces. Extracted so it can be unit-tested
 * directly, without relying on `cy.on('fail')`.
 */
export function assertSnapshotSubjectProduced(
  subject: unknown,
  title: string,
): void {
  if (subject === undefined || subject === null) {
    throw new Error(
      `toMatchSnapshot received ${String(
        subject,
      )} as the subject in "${title}". A snapshot of nothing is a failure — the value under test was not produced.`,
    );
  }
}

export type SnapshotOutcome = 'match' | 'write' | 'missing' | 'diff';

/**
 * Pure decision for how `toMatchSnapshot` should resolve, given the stored-file
 * state and the `updateSnapshots` opt-in. Extracted so the "missing snapshot
 * FAILS (never auto-create-and-pass)" guarantee is unit-tested directly.
 * - `match`   -> already equal, nothing to do.
 * - `write`   -> explicit opt-in, (re)write the file.
 * - `missing` -> no stored baseline and no opt-in -> FAIL.
 * - `diff`    -> stored baseline differs and no opt-in -> FAIL with a diff.
 */
export function decideSnapshotOutcome(args: {
  exists: boolean;
  matches: boolean;
  updateSnapshots: boolean;
}): SnapshotOutcome {
  const { exists, matches, updateSnapshots } = args;
  if (matches) {
    return 'match';
  }
  if (updateSnapshots) {
    return 'write';
  }
  if (!exists) {
    return 'missing';
  }
  return 'diff';
}

function getSnapshotTitle() {
  const testTitle = Cypress.currentTest.titlePath.join(' > ');
  const index = snapshotCounters[testTitle] ?? 0;
  snapshotCounters[testTitle] = index + 1;

  return `${testTitle} #${index}`;
}

Cypress.Commands.add(
  'toMatchSnapshot',
  { prevSubject: true },
  (subject: unknown, options: SnapshotOptions = {}) => {
    // A null/undefined subject is a failure, not a silent pass: it means the
    // value we intended to snapshot was never produced (e.g. a lookup found
    // nothing). (Empty arrays/objects are valid, snapshot-able values.)
    assertSnapshotSubjectProduced(
      subject,
      Cypress.currentTest.titlePath.join(' > '),
    );

    const specFile = Cypress.spec.absolute;
    const snapshotTitle = getSnapshotTitle();
    // Round-trip through JSON so undefined fields are dropped, like in the stored file.
    const actual = normalize(JSON.parse(JSON.stringify(subject)));
    const log = Cypress.log({
      name: 'toMatchSnapshot',
      displayName: 'snapshot',
      message: options.name || snapshotTitle,
      consoleProps: () => ({ snapshotTitle, actual }),
    });

    cy.task('readSnapshot', { specFile, snapshotTitle }, { log: false }).then(
      (result) => {
        const { exists, expected } = result as {
          exists: boolean;
          expected: unknown;
        };

        // Normalize the stored value too, so a pre-existing file with unsorted
        // keys does not produce a false mismatch.
        const normalizedExpected = exists ? normalize(expected) : undefined;
        const matches =
          exists &&
          JSON.stringify(normalizedExpected) === JSON.stringify(actual);

        const outcome = decideSnapshotOutcome({
          exists,
          matches,
          updateSnapshots: Boolean(Cypress.env('updateSnapshots')),
        });

        switch (outcome) {
          case 'match':
            return;

          // Only (re)write snapshots on an explicit opt-in. We never auto-create
          // a missing snapshot and pass — that would hide a missing baseline.
          case 'write':
            log.set(
              'message',
              `${exists ? 'updated' : 'created'}: ${snapshotTitle}`,
            );
            cy.task(
              'writeSnapshot',
              { specFile, snapshotTitle, value: actual },
              { log: false },
            );
            return;

          case 'missing':
            throw new Error(
              `Snapshot "${snapshotTitle}" does not exist. Re-run with \`--env updateSnapshots=true\` to create it, then commit the generated file. Snapshots are not auto-created on normal runs.`,
            );

          case 'diff':
          default:
            // Fails with a readable diff in the Cypress runner.
            expect(actual, `snapshot "${snapshotTitle}"`).to.deep.equal(
              normalizedExpected,
            );
        }
      },
    );
  },
);
