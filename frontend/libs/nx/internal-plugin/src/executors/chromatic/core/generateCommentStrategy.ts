import { chromaticDiagnosticFileSchema } from '../types';

export const CHROMATIC_COMMENT_NEEDLE = '@@:chromaticExecutorComment:@@';

export type GenerateCommentStrategyParams = {
  diagnosticFile: Record<string, unknown> | undefined;
  uncaughtChromaticError: unknown;
  envVars: {
    BUILDKITE_JOB_ID: string | undefined;
    BUILDKITE_BUILD_URL: string | undefined;
  };
};

export type GenerateCommentStrategyResult = {
  status:
    | 'broken'
    | 'denied'
    | 'passed'
    | 'failed'
    | 'pending'
    | 'accepted'
    | 'cancelled'
    | 'noDiagnosticFile'
    | 'wrongDiagnosticFile'
    | 'uncaughtChromaticError';

  error?: Error;

  comment: string;
};

export function generateCommentStrategy(
  params: GenerateCommentStrategyParams
): GenerateCommentStrategyResult {
  const {
    diagnosticFile,
    uncaughtChromaticError,
    envVars: { BUILDKITE_BUILD_URL, BUILDKITE_JOB_ID },
  } = params;

  let signature = `
See the CI job [here](${BUILDKITE_BUILD_URL}#${BUILDKITE_JOB_ID}).

_Sent with 💌 from the frontenders of the Hasura Platform team_.

<!-- ${CHROMATIC_COMMENT_NEEDLE} (allows uniquely identifying the Chromatic->GitHub comment) -->
`;
  if (!BUILDKITE_BUILD_URL || !BUILDKITE_JOB_ID) {
    signature = `
_Sent with 💌 from the frontenders of the Hasura Platform team_.

<!-- ${CHROMATIC_COMMENT_NEEDLE} (allows uniquely identifying the Chromatic->GitHub comment) -->
`;
  }

  if (!diagnosticFile) {
    if (uncaughtChromaticError) {
      const error =
        uncaughtChromaticError instanceof Error
          ? uncaughtChromaticError
          : new Error(
              `Something bad happened to the Chromatic build with an unexpected error (${uncaughtChromaticError})`
            );

      return {
        status: 'uncaughtChromaticError',
        error,
        comment: `
  ### ❌ Chromatic Visual Regression Report
  Something bad happened to the Chromatic build.

  ${signature}`,
      };
    }

    return {
      status: 'noDiagnosticFile',
      error: new Error(
        `CI job is over but the Chromatic diagnostic file cannot be found`
      ),
      comment: `
### 🤔 Chromatic Visual Regression Report
CI job is over but the Chromatic diagnostic file cannot be found. There might be an issue with chromatic, feel free to share this run with the platform team for further diagnostics.

${signature}`,
    };
  }

  // Throws an error if the diagnostic file is invalid
  const diagnosticParseResult =
    chromaticDiagnosticFileSchema.safeParse(diagnosticFile);

  if (diagnosticParseResult.success === false) {
    console.log('Parse error');
    console.log(diagnosticParseResult.error);

    return {
      status: 'wrongDiagnosticFile',
      error: new Error(
        `CI job is over but we are not able to parse the Chromatic diagnostic file. There might be an issue with chromatic, feel free to share this run with the platform team for further diagnostics.`
      ),
      comment: `
### 🤔 Chromatic Visual Regression Report
CI job is over but we are not able to parse the Chromatic diagnostic file. There might be an issue with chromatic, feel free to share this run with the platform team for further diagnostics.

${signature}`,
    };
  }

  const diagnosticData = diagnosticParseResult.data;

  // After parsing it, we are 100% sure the diagnostic file does not contain any secret token that could be leaked
  console.log('Diagnostic file');
  console.log(diagnosticData);

  switch (diagnosticData.build.status) {
    case 'PENDING':
      return {
        status: 'pending',
        error: new Error(
          `Chromatic reported ${diagnosticData.build.changeCount} visual differences with this PR. You can review them [here](${diagnosticData.build.webUrl}).`
        ),
        comment: `
### ⚠️ Chromatic Visual Regression Report
Chromatic reported ${diagnosticData.build.changeCount} visual differences with this PR. You can review them [here](${diagnosticData.build.webUrl}).

If there were intentional changes, you can approve them in Chromatic, and then re run the [\`build-console-storybook\` step](${BUILDKITE_BUILD_URL}#${BUILDKITE_JOB_ID}) on your CI and it should be green!

${signature}`,
      };

    case 'DENIED':
      /*
          At the moment of writing, the DENIED case never happens. When we deny some changes in
          Chromatic and we retry the CI job, Chromatic:
          1. Refuses to re-run the build because it already built the same commit. In this case, the
          diagnostic file is not generated, hence the DENIED case is never reached.
          2. Resets the status to PENDING (if we force Chromatic to re-build with the --force-rebuild
          flag) because a new build means new changes from the Chromatic perspective.
        */
      return {
        status: 'denied',

        // This is the value of this executor: failing the CI when there are visual regressions that
        // have been rejected!
        error: new Error(
          `The ${diagnosticData.build.changeCount} visual differences Chromatic reported with this PR have been rejected. You can review them [here](${diagnosticData.build.webUrl})`
        ),

        comment: `
### ❌ Chromatic Visual Regression Report
The ${diagnosticData.build.changeCount} visual differences Chromatic reported with this PR have been rejected. You can review them [here](${diagnosticData.build.webUrl}).

${signature}`,
      };

    case 'PASSED':
      return {
        status: 'passed',
        comment: `
### ✅ Chromatic Visual Regression Report
Chromatic build passed! You can review it [here](${diagnosticData.build.webUrl}).

${signature}`,
      };

    case 'ACCEPTED':
      return {
        status: 'accepted',
        comment: `
### ✅ Chromatic Visual Regression Report
The ${diagnosticData.build.changeCount} visual differences Chromatic reported with this PR have been accepted. You can view them [here](${diagnosticData.build.webUrl}).

${signature}`,
      };

    case 'BROKEN':
      return {
        status: 'broken',
        error: new Error(
          `There are ${diagnosticData.build.errorCount} errors reported by Chromatic with this PR. You can view them [here](${diagnosticData.build.webUrl}).`
        ),
        comment: `
### ❌ Chromatic Visual Regression Report
Chromatic reported ${diagnosticData.build.errorCount} errors with this PR. You can view them [here](${diagnosticData.build.webUrl}).

${signature}`,
      };

    case 'CANCELLED':
      return {
        status: 'cancelled',
        error: new Error(
          `Chromatic build is broken, maybe something is wrong with some stories. You can view them [here](${diagnosticData.build.webUrl}).`
        ),
        comment: `
### ❌ Chromatic Visual Regression Report
Chromatic build is broken, maybe something is wrong with some stories. You can view them [here](${diagnosticData.build.webUrl}).

${signature}`,
      };

    case 'FAILED':
      return {
        status: 'failed',
        error: new Error(`Something bad happened to the Chromatic build.`),
        comment: `
### ❌ Chromatic Visual Regression Report
Something bad happened to the Chromatic build.

${signature}`,
      };
  }
}
