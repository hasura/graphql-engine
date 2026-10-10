import { useState } from 'react';
import GenerateFilters from './GenerateFilters';
import { FaChevronDown } from 'react-icons/fa';
import FilterSection from './FilterSection';
import FilterBadge from './FilterBadge';
import styles from '../../Metrics.module.scss';
import clsx from 'clsx';
import { Button } from '@hasura/shared/ui';
import { FaFilter } from 'react-icons/fa6';

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
}: any) => {
  const [displayFilters, setFiltersDisplay] = useState(false);
  const renderSelectedFiltersCount = () => {
    return values.length > 0 ? `(${values.length})` : '';
  };
  const resetFilter = () => {
    if (values.length > 0) {
      return (
        <Button size="1" mode="destructive" onClick={reset}>
          Reset all filters
        </Button>
      );
    }
    return null;
  };
  const renderSelectedFilters = () => {
    if (values.length > 0) {
      return values.map((f: { value: any; type: any }, i: any) => {
        const composeFilterObj = (o: { [x: string]: any }) => {
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
            text={`${getTitle(f.type)}: ${filterValue()}`}
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
        <div className={styles['filterBtnWrapper']}>
          <button
            className={clsx('cursor-pointer group mr-2')}
            onClick={toggleFilters}
          >
            <FaChevronDown
              className={clsx(
                'w-8 h-8 p-2 group-hover:bg-slate-200 transition-all duration-150 rounded-full',
                displayFilters ? 'rotate-180' : 'rotate-0',
              )}
            />
          </button>
          <Button
            color="gray"
            leftIcon={FaFilter}
            variant="ghost"
            onClick={toggleFilters}
            className="text-gray-500!"
          >
            <span className="text-gray-800 font-bold">
              Filters {renderSelectedFiltersCount()}
            </span>
          </Button>
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
