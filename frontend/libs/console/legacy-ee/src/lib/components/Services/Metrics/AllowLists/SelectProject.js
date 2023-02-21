import React from 'react';

import { SearchableSelectBox } from '@hasura/console-legacy-ce';

import { useQuery } from '@apollo/react-hooks';

import { createOptionEntry } from './utils';

import { fetchUserProjects } from './graphql.queries';
import { currentProjectPlaceholder } from './constants';

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

const SelectProject = props => {
  const { currentProjectId, selected, onChange } = props;

  const { data } = useQuery(fetchUserProjects);

  const getOptions = () => {
    if (data && data.privilegedProjects && data.privilegedProjects.length > 0) {
      return data.privilegedProjects.map((d, i) => {
        if (d.id === currentProjectId) {
          return createOptionEntry(
            d.id,
            currentProjectPlaceholder,
            currentProjectPlaceholder,
            i
          );
        }
        return createOptionEntry(d.id, d.name, d.name, i);
      });
    }
    return [];
  };

  return (
    <SearchableSelectBox
      options={getOptions()}
      onChange={onChange}
      value={selected}
      styleOverrides={customSelectBoxStyles}
      bsClass="add_table_column_selector"
      filterOption="prefix"
      placeholder="select project"
    />
  );
};

export default SelectProject;
