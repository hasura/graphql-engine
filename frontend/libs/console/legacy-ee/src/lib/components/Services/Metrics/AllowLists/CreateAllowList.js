import { useEffect } from 'react';
import { createOperationGroup } from './graphql.queries';
import { useMutation } from '@apollo/react-hooks';

// const styles = require('../Metrics.module.scss');

/*
const Button = props => {
  const { children } = props;
  return (
    <span>
      <button className={styles.console_save_cta_small} {...props}>
        {children}
      </button>
    </span>
  );
};
*/

const CreateAllowList = ({
  status,
  projectId,
  updateStatus,
  groupName,
  onCreateCompleted,
  projectName,
}) => {
  const onCompleted = () => {
    updateStatus('Fetched successfully');
    onCreateCompleted();
  };
  const [runCreateOGMutation, { loading, error }] = useMutation(
    createOperationGroup,
    {
      variables: {
        operationInfo: {
          project_id: projectId,
          name: groupName,
        },
      },
      onCompleted: onCompleted,
    }
  );
  const runMutation = () => {
    runCreateOGMutation();
  };
  useEffect(runMutation, []);

  const renderBtnText = () => {
    switch (true) {
      case loading:
        return `Getting operations from project(${projectName})`;
      case typeof error === 'object':
        return error.toString();
      case status.length > 0:
        return `${status} ...`;
      default:
        return `Fetch operations from project(${projectName})`;
    }
  };
  return renderBtnText();
  /*
  return (
    <Button onClick={() => runCreateOGMutation()}>{renderBtnText()}</Button>
  );
  */
};

export default CreateAllowList;
