import { useState, Fragment } from 'react';
// import { useMutation } from '@apollo/react-hooks';
import { Button, Dialog } from '@hasura/console-legacy-ce';

import { BrowseRunTests } from './BrowseRunTests';
import { BrowseRunTestsPreview } from './BrowseTestRunPreview';
import styles from '../Metrics.module.scss';

// import { createTestRun } from './graphql.queries';

import Globals from '../../../../Globals';
import { SERVER_CONSOLE_MODE } from '../../../../constants';
import { loadAdminSecretState } from '../../../AppState';

import { getSchemeLuxUrl } from './utils';
import clsx from 'clsx';

/* function which instantiates relevant wasm code and sends them
 * as props to the component
 * */

const withWasm = Component => {
  return props => {
    const go = new window.Go();
    const initWasmInstance = async () => {
      const wasmURL =
        process.env.NODE_ENV !== 'production'
          ? '/rstatic/test-runner.wasm'
          : `${Globals.versionedAssetsPath}/test-runner.wasm.gz`;
      const { instance, module } = await WebAssembly.instantiateStreaming(
        /* TODO: use versioned assets path to make sure console pulls the correct wasm file */
        fetch(wasmURL),
        // fetch('/rstatic/test-runner.wasm'),
        go.importObject
      );
      return {
        instance,
        module,
      };
    };
    return (
      <Component
        {...props}
        initWasmInstance={initWasmInstance}
        goInstance={go}
      />
    );
  };
};

const RunTests = props => {
  const {
    projectId: currentProjectId,
    testSuiteId,
    idToken,
    accessToken = '',
    personalAccessToken = '',
    initWasmInstance,
    goInstance,
    metricsUrl,
    testProjectId,
  } = props;

  // const [testsRunning, setTestsRunning] = useState(false);
  const [testRunId, setTestRunId] = useState(null);
  const [createdAt, setCreatedAt] = useState(null);
  const [showModal, updateModal] = useState(false);

  const [wasmExecution, setWasmExecution] = useState({
    running: false,
    err: null,
    success: null,
  });

  const setWasmExecutionState = updateObj => {
    setWasmExecution(s => {
      return {
        ...s,
        ...updateObj,
      };
    });
  };

  const onHide = () => {
    updateModal(false);
  };

  const { running, err: wasmRunError } = wasmExecution;

  const runTest = async () => {
    setTestRunId(null);
    setWasmExecutionState({ running: true });
    const { instance } = await initWasmInstance();
    const getAdminSecret = () => {
      let adminSecret = '';
      if (Globals.consoleMode === SERVER_CONSOLE_MODE) {
        adminSecret = loadAdminSecretState() || Globals.adminSecret;
      } else {
        adminSecret = Globals.adminSecret;
      }
      return adminSecret;
    };

    window.onSuccess = () => {};
    window.onError = err => {
      setWasmExecutionState({ running: false, err });
    };
    window.getIDTokenFn = () => idToken || '';
    window.getAccessTokenFn = () => accessToken || '';
    window.onCreated = d => {
      setWasmExecutionState({ running: false, success: d });
      // Set the data
      setTestRunId(d);
      setCreatedAt(new Date());
    };
    const { url, scheme } = getSchemeLuxUrl();
    goInstance.env = {
      rootDomain: url,
      scheme: scheme,
      testSuiteID: testSuiteId,
      pat: personalAccessToken || '',
      onCreated: 'onCreated',
      onSuccess: 'onSuccess',
      onError: 'onError',
      graphqlEndpoint: `${Globals.dataApiUrl}/v1/graphql`,
      getIDTokenFn: 'getIDTokenFn',
      getAccessTokenFn: 'getAccessTokenFn',
      adminSecret: getAdminSecret(),
      metricsHost: metricsUrl,
    };
    await goInstance.run(instance);
  };

  const getRunBtnText = () => {
    if (running) {
      return 'Running...';
    } else if (wasmRunError) {
      setTimeout(() => setWasmExecutionState({ err: null }), 5000);
      return wasmRunError.toString();
    }
    return 'Run tests now';
  };

  return (
    <Fragment>
      <div className={`${styles.displayInline}`}>
        <Button
          className={`${styles.displayInline} ${styles.runTestsButton}}`}
          size="sm"
          disabled={running}
          onClick={() => {
            runTest();
          }}
        >
          {getRunBtnText()}
        </Button>
        <Button
          className={`${styles.displayInline} ${styles.runTestsButton} ml-0.5`}
          size="sm"
          disabled={running}
          onClick={() => {
            updateModal(true);
          }}
        >
          Run tests on CLI
        </Button>
      </div>
      {showModal && (
        <Dialog
          id="operationInspect"
          onClose={onHide}
          hasBackdrop
          size="xxl"
          title="Run tests from CLI"
        >
          <div className={`p-sm ${styles.modalWrapper}`}>
            <div className={clsx(styles.modalContainer, 'flex flex-col')}>
              <div className={clsx(' bg-slate-50 px-2 pb-4 pt-0')}>
                You can run tests from the CLI also. See{' '}
                <a
                  href="https://docs.pro.hasura.io/cli/regression-tests/"
                  target="_blank"
                  rel="noopener noreferrer"
                >
                  docs
                </a>{' '}
                for more info.
              </div>
              <code>
                {`hasura pro regression-tests run --testsuite-id ${testSuiteId} --project-id ${currentProjectId}`}
              </code>
            </div>
          </div>
        </Dialog>
      )}
      {testRunId ? (
        <BrowseRunTests
          testSuiteId={testSuiteId}
          testRunId={testRunId}
          createdAt={createdAt}
        />
      ) : (
        <BrowseRunTestsPreview
          projectId={testProjectId}
          testSuiteId={testSuiteId}
        />
      )}
    </Fragment>
  );
};

export default withWasm(RunTests);
