import React, { useState } from 'react';

import { SearchableSelectBox } from '@hasura/console-legacy-ce';

import AddOperationsToAllowList from './AddOperationsToAllowList';

import { useQuery } from '@apollo/react-hooks';

import { createOptionEntry } from './utils';

import {
  fetchUserProjects,
  fetchAllRemoteAllowedOperations,
} from './graphql.queries';

import LoadingSpinner from '../Common/LoadingSpinner';

import styles from '../Metrics.module.scss';

/* Custom style object for searchable select box */
const customSelectBoxStyles = {
  dropdownIndicator: {
    padding: '5px',
  },
  singleValue: {
    color: '#555555',
  },
  valueContainer: {
    padding: '0px 12px',
  },
};

const SpacedSpan = ({ children }) => {
  return (
    <span className={styles.displayInline + ' ' + styles.leftSpaced}>
      {children}
    </span>
  );
};

const FetchProjects = ({ projectId, groupName, children }) => {
  const variables = {
    projectId: projectId,
    groupName: groupName,
  };
  const { loading, error, data } = useQuery(fetchAllRemoteAllowedOperations, {
    variables,
    fetchPolicy: 'network-only',
  });
  if (loading) {
    return (
      <SpacedSpan>
        <LoadingSpinner />
      </SpacedSpan>
    );
  }
  if (error) {
    return (
      <SpacedSpan>Error fetching operations: ${error.toString()}</SpacedSpan>
    );
  }
  return children(data);
};

const SelectProjectAndImport = props => {
  const [projectState, updateProjectState] = useState({});

  const { projectId } = projectState;

  const updateProjectStateCb = selectedOption => {
    updateProjectState({
      projectName: selectedOption.label,
      projectId: selectedOption.value,
    });
  };

  const { refetch, groupName, dispatch, currentProjectId } = props;

  const { data } = useQuery(fetchUserProjects);

  const getOptions = () => {
    if (data && data.privilegedProjects && data.privilegedProjects.length > 0) {
      const validRemoteProjects = data.privilegedProjects.filter(
        f => f.id !== currentProjectId
      );
      if (validRemoteProjects.length > 0) {
        return validRemoteProjects.map((d, i) => {
          return createOptionEntry(d.id, d.name, d.name, i);
        });
      }
      return [];
    }
    return [];
  };

  const selectedValue = () => {
    if (Object.keys(projectState).length > 0) {
      return createOptionEntry(
        projectState.projectId,
        projectState.projectName,
        projectState.projectName
      );
    }
    return null;
  };

  const renderBody = () => {
    if (projectId) {
      return (
        <FetchProjects groupName={groupName} projectId={projectId}>
          {operations => {
            if (operations.results.length > 0) {
              return (
                <div className={styles.displayInline + ' ' + styles.addBtn}>
                  <AddOperationsToAllowList
                    operations={operations.results}
                    dispatch={dispatch}
                    collectionName={groupName}
                    projectId={currentProjectId}
                    onCompletedCb={() => refetch()}
                    btnStates={{
                      btnLoading: 'Importing...',
                      btnSuccessful: 'Import Successful',
                      btnInit: 'Import',
                    }}
                  />
                </div>
              );
            }
            return (
              <SpacedSpan>
                <span className={styles.noOperations}>
                  * No operations available to import
                </span>
              </SpacedSpan>
            );
          }}
        </FetchProjects>
      );
    }
    return null;
  };

  return (
    <div className={styles.searchProjectAndImport + ' ' + styles.displayInline}>
      <span className={styles.displayInline + ' ' + styles.searchBar}>
        <SearchableSelectBox
          placeholder="select project"
          options={getOptions()}
          onChange={updateProjectStateCb}
          value={selectedValue()}
          styleOverrides={customSelectBoxStyles}
          bsClass="add_table_column_selector"
          filterOption={'prefix'}
        />
      </span>
      {renderBody()}
    </div>
  );
};

export default SelectProjectAndImport;
