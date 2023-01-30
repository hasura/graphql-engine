import React, { useState } from 'react';

import { ContentWithTestSuite } from './ContentWithTestSuite';
import RunTests from './RunTests';
import styles from '../Metrics.module.scss';
import SelectProject from '../AllowLists/SelectProject';
import { currentProjectPlaceholder } from '../AllowLists/constants';
import { createOptionEntry } from '../AllowLists/utils';

/*
 * Lists projects to choose the test suite from
 *  If the project has a testSuiteId, show the button, else show an error saying
 * */

export const RunTestsWrapper = props => {
  const { projectId: currentProjectId, projectName: currentProjectName } =
    props;

  const [project, setProject] = useState({
    projectName: currentProjectName,
    projectId: currentProjectId,
  });

  const handleProjectChange = selected => {
    setProject({
      projectId: selected.value,
      projectName: selected.label,
    });
  };

  const selectedProjectName =
    project.projectId === currentProjectId
      ? currentProjectPlaceholder
      : project.projectName;

  const selectedProjectInfo = createOptionEntry(
    project.projectId,
    selectedProjectName,
    selectedProjectName
  );

  return (
    <div>
      <p>Pick the project to load tests from</p>
      <div className={`${styles.pickProjectDropdown} ${styles.displayInline}`}>
        <SelectProject
          selected={selectedProjectInfo}
          onChange={handleProjectChange}
          currentProjectId={currentProjectId}
        />
      </div>
      <span className={styles.componentLeftSpacing}>
        <ContentWithTestSuite
          {...props}
          {...project}
          currentProjectId={currentProjectId}
        >
          {({ testSuiteId, testProjectId }) => (
            <RunTests
              {...props}
              testSuiteId={testSuiteId}
              testProjectId={testProjectId}
            />
          )}
        </ContentWithTestSuite>
      </span>
    </div>
  );
};
