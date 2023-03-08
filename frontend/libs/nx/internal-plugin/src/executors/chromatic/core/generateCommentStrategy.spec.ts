import produce from 'immer';

import type { ChromaticDiagnosticFile } from '../types';
import type {
  GenerateCommentStrategyParams,
  GenerateCommentStrategyResult,
} from './generateCommentStrategy';

import { generateCommentStrategy } from './generateCommentStrategy';

const happyPathDiagnosticFile: ChromaticDiagnosticFile = {
  build: {
    status: 'PASSED',
    changeCount: 0,
    errorCount: 0,
    webUrl:
      'https://www.chromatic.com/build?appId=614d7904644d03004addd43b&number=8192',
  },
};

const happyPathParams: GenerateCommentStrategyParams = {
  diagnosticFile: happyPathDiagnosticFile,
  uncaughtChromaticError: undefined,
  envVars: {
    BUILDKITE_JOB_ID: '01867899-412e-4b24-bb8f-545e2a91e1f1',
    BUILDKITE_BUILD_URL:
      'https://buildkite.com/hasura/graphql-engine-mono/builds/30019',
  },
};

describe('generateCommentStrategy', () => {
  it('When passed with the happy path params, it shuold return the "passed" comment strategy', () => {
    const expected: GenerateCommentStrategyResult = {
      comment: `
### ✅ Chromatic Visual Regression Report
Chromatic build passed! You can review it [here](https://www.chromatic.com/build?appId=614d7904644d03004addd43b&number=8192).


See the CI job [here](https://buildkite.com/hasura/graphql-engine-mono/builds/30019#01867899-412e-4b24-bb8f-545e2a91e1f1).

_Sent with 💌 from the frontenders of the Hasura Platform team_.

<!-- @@:chromaticExecutorComment:@@ (allows uniquely identifying the Chromatic->GitHub comment) -->
`,
      status: 'passed',
    };

    const result = generateCommentStrategy(happyPathParams);
    expect(result).toEqual(expected);
  });

  describe('No diagnostic file', () => {
    it('When passed without a diagnostic file, it should return the "noDiagnosticFile" comment strategy', () => {
      const params = produce(happyPathParams, draft => {
        draft.diagnosticFile = undefined;
      });

      const result = generateCommentStrategy(params);

      expect(result.status).toEqual('noDiagnosticFile');
      expect(result.comment).toEqual(
        expect.stringContaining('### 🤔 Chromatic Visual Regression Report')
      );
      expect(result.comment).toEqual(
        expect.stringContaining(
          'CI job is over but the Chromatic diagnostic file cannot be found. There might be an issue with chromatic, feel free to share this run with the platform team for further diagnostics.'
        )
      );
      expect(result.error).toEqual(
        new Error(
          'CI job is over but the Chromatic diagnostic file cannot be found'
        )
      );
    });

    it('When passed without a diagnostic file and an uncaught error from Chromatic, it should return the "uncaughtChromaticError" comment strategy', () => {
      const params = produce(happyPathParams, draft => {
        draft.diagnosticFile = undefined;
        draft.uncaughtChromaticError = new Error('Something bad happened');
      });

      const result = generateCommentStrategy(params);

      expect(result.status).toEqual('uncaughtChromaticError');
      expect(result.comment).toEqual(
        expect.stringContaining('### ❌ Chromatic Visual Regression Report')
      );
      expect(result.comment).toEqual(
        expect.stringContaining(
          'Something bad happened to the Chromatic build.'
        )
      );
      expect(result.error).toEqual(new Error('Something bad happened'));
    });

    it('When passed without a diagnostic file and something strange from Chromatic, it should return the "uncaughtChromaticError" comment strategy', () => {
      const params = produce(happyPathParams, draft => {
        draft.diagnosticFile = undefined;
        draft.uncaughtChromaticError = 'a Strange Chromatic CLI error';
      });

      const result = generateCommentStrategy(params);

      expect(result.status).toEqual('uncaughtChromaticError');
      expect(result.comment).toEqual(
        expect.stringContaining('### ❌ Chromatic Visual Regression Report')
      );
      expect(result.comment).toEqual(
        expect.stringContaining(
          'Something bad happened to the Chromatic build.'
        )
      );
      expect(result.error).toEqual(
        new Error(
          'Something bad happened to the Chromatic build with an unexpected error (a Strange Chromatic CLI error)'
        )
      );
    });
  });

  describe('Invalid diagnostic file', () => {
    it('When passed with an invalid diagnostic file, it should return the "wrongDiagnosticFile" comment strategy', () => {
      const params = produce(happyPathParams, draft => {
        draft.diagnosticFile = {};
      });

      const result = generateCommentStrategy(params);

      expect(result.status).toEqual('wrongDiagnosticFile');
      expect(result.comment).toEqual(
        expect.stringContaining('### 🤔 Chromatic Visual Regression Report')
      );
      expect(result.comment).toEqual(
        expect.stringContaining(
          'CI job is over but we are not able to parse the Chromatic diagnostic file. There might be an issue with chromatic, feel free to share this run with the platform team for further diagnostics.'
        )
      );
      expect(result.error).toEqual(
        new Error(
          'CI job is over but we are not able to parse the Chromatic diagnostic file. There might be an issue with chromatic, feel free to share this run with the platform team for further diagnostics.'
        )
      );
    });

    it('When passed with an unknown diagnostic file, it should return the "wrongDiagnosticFile" comment strategy', () => {
      const params = produce(happyPathParams, paramsDraft => {
        paramsDraft.diagnosticFile = produce(happyPathDiagnosticFile, draft => {
          // @ts-expect-error Setting an unknown chromatic status on purpose
          draft.build.status = 'UNMANAGED_CHROMATIC_STATUS';
        });
      });

      const result = generateCommentStrategy(params);

      expect(result.status).toEqual('wrongDiagnosticFile');
      expect(result.comment).toEqual(
        expect.stringContaining('### 🤔 Chromatic Visual Regression Report')
      );
      expect(result.comment).toEqual(
        expect.stringContaining(
          'CI job is over but we are not able to parse the Chromatic diagnostic file. There might be an issue with chromatic, feel free to share this run with the platform team for further diagnostics.'
        )
      );
      expect(result.error).toEqual(
        new Error(
          'CI job is over but we are not able to parse the Chromatic diagnostic file. There might be an issue with chromatic, feel free to share this run with the platform team for further diagnostics.'
        )
      );
    });
  });

  it('When passed with a PENDING diagnostic file, it should return the "pending" comment strategy', () => {
    const params = produce(happyPathParams, paramsDraft => {
      paramsDraft.diagnosticFile = produce(happyPathDiagnosticFile, draft => {
        draft.build.status = 'PENDING';
        draft.build.changeCount = 5;
      });
    });

    const result = generateCommentStrategy(params);

    expect(result.status).toEqual('pending');
    expect(result.comment).toEqual(
      expect.stringContaining('### ⚠️ Chromatic Visual Regression Report')
    );
    expect(result.comment).toEqual(
      expect.stringContaining(
        'Chromatic reported 5 visual differences with this PR'
      )
    );
    expect(result.error).toEqual(
      new Error(
        'Chromatic reported 5 visual differences with this PR. You can review them [here](https://www.chromatic.com/build?appId=614d7904644d03004addd43b&number=8192).'
      )
    );
  });

  it('When passed with a DENIED diagnostic file, it should return the "denied" comment strategy', () => {
    const params = produce(happyPathParams, paramsDraft => {
      paramsDraft.diagnosticFile = produce(happyPathDiagnosticFile, draft => {
        draft.build.status = 'DENIED';
        draft.build.changeCount = 5;
      });
    });

    const result = generateCommentStrategy(params);

    expect(result.status).toEqual('denied');
    expect(result.comment).toEqual(
      expect.stringContaining('### ❌ Chromatic Visual Regression Report')
    );
    expect(result.comment).toEqual(
      expect.stringContaining(
        'The 5 visual differences Chromatic reported with this PR have been rejected'
      )
    );
    expect(result.error).toEqual(
      new Error(
        'The 5 visual differences Chromatic reported with this PR have been rejected. You can review them [here](https://www.chromatic.com/build?appId=614d7904644d03004addd43b&number=8192)'
      )
    );
  });

  it('When passed with a PASSED diagnostic file, it should return the "passed" comment strategy', () => {
    const params = produce(happyPathParams, paramsDraft => {
      paramsDraft.diagnosticFile = produce(happyPathDiagnosticFile, draft => {
        draft.build.status = 'PASSED';
      });
    });

    const result = generateCommentStrategy(params);

    expect(result.status).toEqual('passed');
    expect(result.comment).toEqual(
      expect.stringContaining('### ✅ Chromatic Visual Regression Report')
    );
    expect(result.comment).toEqual(
      expect.stringContaining('Chromatic build passed!')
    );
  });

  it('When passed with a ACCEPTED diagnostic file, it should return the "accepted" comment strategy', () => {
    const params = produce(happyPathParams, paramsDraft => {
      paramsDraft.diagnosticFile = produce(happyPathDiagnosticFile, draft => {
        draft.build.status = 'ACCEPTED';
        draft.build.changeCount = 5;
      });
    });

    const result = generateCommentStrategy(params);

    expect(result.status).toEqual('accepted');
    expect(result.comment).toEqual(
      expect.stringContaining('### ✅ Chromatic Visual Regression Report')
    );
    expect(result.comment).toEqual(
      expect.stringContaining(
        'The 5 visual differences Chromatic reported with this PR have been accepted.'
      )
    );
  });

  it('When passed with a BROKEN diagnostic file, it should return the "broken" comment strategy', () => {
    const params = produce(happyPathParams, paramsDraft => {
      paramsDraft.diagnosticFile = produce(happyPathDiagnosticFile, draft => {
        draft.build.status = 'BROKEN';
        draft.build.errorCount = 3;
      });
    });

    const result = generateCommentStrategy(params);

    expect(result.status).toEqual('broken');
    expect(result.comment).toEqual(
      expect.stringContaining('### ❌ Chromatic Visual Regression Report')
    );
    expect(result.comment).toEqual(
      expect.stringContaining('Chromatic reported 3 errors with this PR')
    );
    expect(result.error).toEqual(
      new Error(
        'There are 3 errors reported by Chromatic with this PR. You can view them [here](https://www.chromatic.com/build?appId=614d7904644d03004addd43b&number=8192).'
      )
    );
  });

  it('When passed with a CANCELLED diagnostic file, it should return the "cancelled" comment strategy', () => {
    const params = produce(happyPathParams, paramsDraft => {
      paramsDraft.diagnosticFile = produce(happyPathDiagnosticFile, draft => {
        draft.build.status = 'CANCELLED';
      });
    });

    const result = generateCommentStrategy(params);

    expect(result.status).toEqual('cancelled');
    expect(result.comment).toEqual(
      expect.stringContaining('### ❌ Chromatic Visual Regression Report')
    );
    expect(result.comment).toEqual(
      expect.stringContaining(
        'Chromatic build is broken, maybe something is wrong with some stories.'
      )
    );
    expect(result.error).toEqual(
      new Error(
        'Chromatic build is broken, maybe something is wrong with some stories. You can view them [here](https://www.chromatic.com/build?appId=614d7904644d03004addd43b&number=8192).'
      )
    );
  });

  it('When passed with a FAILED diagnostic file, it should return the "failed" comment strategy', () => {
    const params = produce(happyPathParams, paramsDraft => {
      paramsDraft.diagnosticFile = produce(happyPathDiagnosticFile, draft => {
        draft.build.status = 'FAILED';
      });
    });

    const result = generateCommentStrategy(params);

    expect(result.status).toEqual('failed');
    expect(result.comment).toEqual(
      expect.stringContaining('### ❌ Chromatic Visual Regression Report')
    );
    expect(result.comment).toEqual(
      expect.stringContaining('Something bad happened to the Chromatic build.')
    );
    expect(result.error).toEqual(
      new Error('Something bad happened to the Chromatic build.')
    );
  });
});
