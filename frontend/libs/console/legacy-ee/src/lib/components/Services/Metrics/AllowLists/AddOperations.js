import React, { useState, useEffect } from 'react';

import AllowListDataWrapper from './AllowListDataWrapper';

import ManageAllowList from './ManageAllowList';

import BrowserRows from './BrowseRows';

import SelectProject from './SelectProject';

import { currentProjectPlaceholder } from './constants';

import { createOptionEntry } from './utils';

import styles from '../Metrics.module.scss';

// Type info
// addToAllowList is a list of objects of the type:
//  {
//    name: String, //OperationName
//    query: String, // Query string of the operation
//    operationId: uuid // Unique id for the operation
//  }

const defaultState = {
  addToAllowList: [],
};

const AddOperations = props => {
  const [addOperationS, update] = useState(defaultState);

  const [projectState, updateProjectState] = useState({});

  const { addToAllowList } = addOperationS;

  const updateAllowList = operation => {
    const elementPos = addToAllowList.findIndex(o => {
      return o.name === operation.name;
    });
    if (elementPos !== -1) {
      update(s => {
        return {
          ...s,
          addToAllowList: [
            ...s.addToAllowList.slice(0, elementPos),
            ...s.addToAllowList.slice(elementPos + 1),
          ],
        };
      });
      return;
    }
    update(s => {
      return {
        ...s,
        addToAllowList: [...s.addToAllowList, operation],
      };
    });
  };

  const toggleAddClearAllowList = operations => {
    update(s => {
      return {
        ...s,
        addToAllowList: [...operations],
      };
    });
  };

  const { collectionName, RenderLink } = props;

  const initializeSelectProjectState = () => {
    updateProjectState({
      projectName: props.projectName,
      projectId: props.projectId,
    });
  };

  const updateProjectStateCb = selectedOption => {
    updateProjectState({
      projectName: selectedOption.label,
      projectId: selectedOption.value,
    });
  };

  const { projectName, projectId } = projectState;

  const { projectId: currentProjectId } = props;

  useEffect(initializeSelectProjectState, []);

  const getProjectName = () => {
    if (projectId === currentProjectId) {
      return currentProjectPlaceholder;
    }
    return projectName;
  };

  const renderBody = () => {
    const selectedProjectInfo = createOptionEntry(
      projectId,
      getProjectName(),
      getProjectName()
    );
    return (
      <AllowListDataWrapper projectId={projectId} groupName={collectionName}>
        {({ data, refetch }) => {
          const { operation_groups: operationGroups } = data;
          const renderNext = () => {
            return (
              <div>
                <ManageAllowList
                  operationGroups={operationGroups}
                  projectId={projectId}
                  refetch={refetch}
                  projectName={projectName}
                  shouldCreateOperationGroup={
                    operationGroups.length === 0 &&
                    currentProjectId === projectId
                  }
                >
                  {() => {
                    return (
                      <React.Fragment>
                        <div>
                          Showing operations from
                          <div
                            className={
                              styles.selectProject + ' ' + styles.displayInline
                            }
                          >
                            <SelectProject
                              selected={selectedProjectInfo}
                              onChange={updateProjectStateCb}
                              currentProjectId={currentProjectId}
                            />
                          </div>
                        </div>
                        <BrowserRows
                          {...props}
                          label={''}
                          projectId={currentProjectId}
                          remoteProjectId={projectId}
                          projectName={projectName}
                          RenderLink={RenderLink}
                          allowlist={addToAllowList}
                          updateAllowList={updateAllowList}
                          toggleAddClearAllowList={toggleAddClearAllowList}
                        />
                      </React.Fragment>
                    );
                  }}
                </ManageAllowList>
              </div>
            );
          };
          return renderNext();
        }}
      </AllowListDataWrapper>
    );
  };

  return renderBody();
};

export default AddOperations;
