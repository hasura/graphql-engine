import React, { useState } from 'react';
import GenerateFilters from './GenerateFilters';
import { Button } from '@hasura/console-oss';
import FilterSection from './FilterSection';
import FilterBadge from './FilterBadge';
import filter from '../../images/filter.svg';

import styles from '../../Metrics.module.scss';

const Filters = ({
  projectId,
  query,
  retrieveFilterData,
  getTitle,
  getEmptyTitle,
  onChange,
  filters,
  values,
  retrieveDefaultDropdownOptions,
  reset,
  selectAll,
}) => {
  const [displayFilters, setFiltersDisplay] = useState(false);
  const renderSelectedFiltersCount = () => {
    return values.length > 0 ? `(${values.length})` : '';
  };
  const resetFilter = () => {
    if (values.length > 0) {
      return (
        <Button mode="destructive" onClick={reset}>
          Remove all filters
        </Button>
      );
    }
    return null;
  };
  const renderSelectedFilters = () => {
    if (values.length > 0) {
      return values.map((f, i) => {
        const composeFilterObj = (o) => {
          const keyElements = Object.keys(o);
          if (keyElements.length > 0) {
            return keyElements.map((k) => `${k}: ${o[k]}`).join(', ');
          }
          return 'N/A';
        };
        const filterValue = () => {
          if (typeof f.value === 'string') {
            return f.value;
          }
          return composeFilterObj(f.value);
        };
        return (
          <FilterBadge
            key={i}
            text={`${getTitle(f.type)}: ${filterValue(f.value)}`}
            onClick={() => onChange(f.type, f.value)}
          />
        );
      });
    }
    return null;
  };

  const toggleFilters = () => {
    setFiltersDisplay(!displayFilters);
  };

  return (
    <div className="filtersElementWrapper">
      <FilterSection>
        <div className={styles.filterBtnWrapper}>
          <div onClick={toggleFilters} className={styles.cursorPointer}>
            <img
              src={filter}
              alt="Filter"
              style={{
                marginRight: '8px',
                marginBottom: '3px',
                width: '20px',
                height: '20px',
              }}
            />
            <div className={styles.subHeader}>
              Filters {renderSelectedFiltersCount()}
            </div>
          </div>
          {renderSelectedFilters()}
          {resetFilter()}
        </div>
        {displayFilters && (
          <GenerateFilters
            projectId={projectId}
            query={query}
            retrieveFilterData={retrieveFilterData}
            retrieveDefaultDropdownOptions={retrieveDefaultDropdownOptions}
            getTitle={getTitle}
            getEmptyTitle={getEmptyTitle}
            onChange={onChange}
            filters={filters}
            values={values}
            selectAll={selectAll}
          />
        )}
      </FilterSection>
    </div>
  );
};

export default Filters;
