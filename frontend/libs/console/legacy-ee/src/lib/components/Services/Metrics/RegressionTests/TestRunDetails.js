import { useState, Fragment } from 'react';
import { Dialog } from '@hasura/console-legacy-ce';
import { useQuery } from '@apollo/react-hooks';

import CustomCopy from '../Common/CustomCopy';
// import editIcon from '../images/edit.svg';
import lensIcon from '../images/usage.svg';
import styles from '../Metrics.module.scss';
import { fetchTestRunDetailsByName } from './graphql.queries';

import { IN_PROCESS_ENUM } from './constants';

/**
 * @typedef Props
 * @property {string} testRunId
 * @property {string} operationName
 *
 * @param {Props} props
 */
export const TestRunDetails = ({ status, testRunId, operationName }) => {
  const [modalOpen, setModalOpen] = useState(false);
  const renderDetails = () => {
    return modalOpen ? (
      <TestRunDetailsModal
        show={modalOpen}
        onHide={() => setModalOpen(false)}
        testRunId={testRunId}
        operationName={operationName}
      />
    ) : null;
  };

  const renderDetailIcon = () => {
    if (status !== IN_PROCESS_ENUM) {
      return (
        <div className={styles.textCenter}>
          <img
            className={styles.actionImg}
            src={lensIcon}
            alt="Test details"
            onClick={() => setModalOpen(true)}
          />
          {renderDetails()}
        </div>
      );
    }
    return null;
  };

  return renderDetailIcon();
};

/**
 * @typedef Props
 * @property {boolean} show
 * @property {() => void} onHide
 * @property {string} testRunId
 * @property {string} operationName
 *
 * @param {Props} props
 */
const TestRunDetailsModal = ({ onHide, show, operationName, testRunId }) => {
  const { loading, error, data } = useQuery(fetchTestRunDetailsByName, {
    variables: {
      operationName,
      testRunId,
    },
  });

  if (!show) return null;

  return (
    <Dialog
      id="operationInspect"
      onClose={onHide}
      hasBackdrop
      size="xxl"
      title="Test details"
    >
      <div className={`p-md ${styles.modalWrapper}`}>
        <div className={styles.modalContainer}>
          {loading && <LoadingBody />}
          {error && <ErrorBody error={error} />}
          {data && data.test_run_details && data.test_run_details.length && (
            <Fragment>
              <div
                className={`col-md-6 ${styles.noPadd} ${styles.borderRight} ${styles.halfWidth}`}
              >
                <CustomCopy
                  key="graphql-query"
                  label="Query"
                  copy={data.test_run_details[0].query}
                />
                <CustomCopy
                  key="session_variables"
                  label="Session variables"
                  copy={JSON.stringify(
                    data.test_run_details[0].user_vars,
                    null,
                    2
                  )}
                />
                <CustomCopy
                  key="variables"
                  label="Variables"
                  copy={JSON.stringify(
                    data.test_run_details[0].variables,
                    null,
                    2
                  )}
                />
              </div>
              <div
                className={`col-md-6 ${styles.noPadd} ${styles.halfWidth} ${styles.longCopy}`}
              >
                <CustomCopy
                  key="response"
                  label="Response"
                  copy={JSON.stringify(
                    data.test_run_details[0].response,
                    null,
                    2
                  )}
                />
              </div>
            </Fragment>
          )}
        </div>
      </div>
    </Dialog>
  );
};

const LoadingBody = () => <span>Fetching test run details...</span>;

const ErrorBody = ({ error }) => (
  <span className={styles.errorMessage}>
    Error fetching
    <code>{error.toString()}</code>
  </span>
);
