import { useState, Fragment } from 'react';
import { Dialog, Tooltip } from '@hasura/console-legacy-ce';
import { useMutation } from '@apollo/react-hooks';

import CustomCopy from '../Common/CustomCopy';
import editIcon from '../images/edit.svg';
import styles from '../Metrics.module.scss';
import { EditData } from './EditData';
import { useQuery } from 'react-apollo';
import {
  updateVariables,
  fetchTestSuiteOperationByName,
} from './graphql.queries';

/**
 * @typedef Props
 * @property {string} testSuiteId
 * @property {string} name
 *
 * @param {Props} props
 */
export const EditRow = ({ name, testSuiteId }) => {
  const [modalOpen, setModalOpen] = useState(false);

  const tooltipText = 'Edit';

  return (
    <Fragment>
      <Tooltip side="right" tooltipContentChildren={tooltipText}>
        <img
          className={styles.actionImg + ' ' + styles.addPaddingRight}
          src={editIcon}
          alt="Edit"
          onClick={() => setModalOpen(true)}
        />
      </Tooltip>
      <EditRowModal
        testSuiteId={testSuiteId}
        show={modalOpen}
        onHide={() => setModalOpen(false)}
        name={name}
      />
    </Fragment>
  );
};

/**
 * @typedef Props
 * @property {string} testSuiteId
 * @property {boolean} show
 * @property {() => void} onHide
 * @property {string} name
 *
 * @param {Props} props
 */
const EditRowModal = ({ onHide, show, name, testSuiteId }) => {
  const [isEditing, setIsEditing] = useState(false);

  const { loading, error, data, refetch } = useQuery(
    fetchTestSuiteOperationByName,
    {
      variables: {
        testSuiteId,
        opsName: name,
      },
    }
  );

  const [updateOperationVariables] = useMutation(updateVariables);

  const handleSave = newVariables => {
    updateOperationVariables({
      variables: {
        testSuiteId,
        variables: JSON.parse(newVariables),
        opsName: name,
      },
    }).then(
      () => {
        setIsEditing(false);
        refetch();
      },
      err => alert(err.toString())
    );
  };

  const renderModalBody = () => {
    if (data && data.test_suite_operations) {
      const { query, variables, user_vars } = data.test_suite_operations[0];
      return (
        <Fragment>
          <div
            className={`col-md-6 ${styles.noPadd} ${styles.borderRight} ${styles.paddingBottom}`}
          >
            <CustomCopy key="graphql-query" label="Query" copy={query} />
            <CustomCopy
              key="session_variables"
              label="Session variables"
              copy={JSON.stringify(user_vars, null, 2)}
            />
          </div>
          <div
            className={`col-md-6 ${styles.noPadd} ${styles.longCopy} ${styles.paddingBottom} w-full`}
          >
            {isEditing ? (
              <EditData
                label="Variables"
                data={JSON.stringify(variables, null, 2)}
                onSave={values => {
                  handleSave(values);
                  setIsEditing(false);
                }}
                onCancel={() => setIsEditing(false)}
              />
            ) : (
              <CustomCopy
                key="variables"
                copy={JSON.stringify(variables, null, 2)}
                label="Variables"
                onEdit={() => setIsEditing(true)}
              />
            )}
          </div>
        </Fragment>
      );
    }
    if (loading) {
      return <span>'Fetching data...'</span>;
    }
    if (error) {
      return (
        <span className={styles.errorMessage}>
          Error fetching
          <code>{error.toString()}</code>
        </span>
      );
    }
  };

  if (!show) return null;

  return (
    <Dialog
      id="operationInspect"
      onClose={onHide}
      hasBackdrop
      size="xxl"
      title="Inspect"
    >
      <div className={styles.modalWrapper}>
        <div className={styles.modalContainer}>{renderModalBody()}</div>
      </div>
    </Dialog>
  );
};
