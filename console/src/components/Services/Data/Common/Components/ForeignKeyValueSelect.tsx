import React, {
  useState,
  useEffect,
  useRef,
  useMemo,
  ComponentProps,
} from 'react';
import debounce from 'lodash.debounce';

import styles from '../../../../Common/TableCommon/Table.scss';
import SearchableSelect from '../../../../Common/SearchableSelect/SearchableSelect';
import { DisplayNameSelect } from './DisplayNameSelect';
import { ReftablesType } from './TypedInput';
import {
  upsertLocalStorageObject,
  getLocalStorageItem,
} from '../../../../Common/utils/localStorageUtils';

const DISPLAY_NAME_PREF_KEY = 'DISPLAY_NAME_PREF';

const searchableSelectStyles = {
  container: {
    width: '270px',
  },
  control: {
    minHeight: '34px',
  },
  dropdownIndicator: {
    padding: '5px',
  },
  valueContainer: {
    padding: '0px 12px',
  },
};

const createOpt = (prevValue: string) => ({
  value: prevValue,
  label: prevValue,
});

type Option = { label: string; value: string };

export type FkColOption = {
  from: string;
  to: string;
  displayName: string;
  refTable?: string;
  data: Array<Record<string, string>>;
};

type Props = {
  prevValue: string;
  fkOptions: Array<FkColOption>;
  getFkOptions: (opts: FkColOption, value: string) => Promise<void>;
  onFkValueChange: ComponentProps<typeof SearchableSelect>['onChange'];
  selectedOption: Option;
  standardInputProps: ComponentProps<'select'>;
  placeholder: string;
  columnName: string;
  refTables: ReftablesType;
  foreignKey: any;
  wrapperClassName: string;
};
const getLastPref = (constrainName: string) => {
  const str = getLocalStorageItem(DISPLAY_NAME_PREF_KEY) || '{}';
  return JSON.parse(str)[constrainName];
};
export const ForeignKeyValueSelect: React.FC<Props> = ({
  prevValue,
  fkOptions,
  getFkOptions,
  onFkValueChange,
  selectedOption,
  standardInputProps,
  columnName,
  placeholder,
  refTables,
  foreignKey,
  children,
  wrapperClassName,
}) => {
  const columnFkOpts = useRef<FkColOption>();
  const displayNames =
    refTables[foreignKey.ref_table || ''].filter(dnOpt =>
      ['text', 'citext', 'varchar'].includes(dnOpt.type)
    ) || [];
  const [displayName, setDisplayName] = useState(
    getLastPref(foreignKey.constraint_name) || ''
  );
  columnFkOpts.current = (fkOptions &&
    fkOptions.find(opts => opts.from === columnName)) || {
    from: foreignKey.columns[0],
    to: foreignKey.ref_columns[0],
    displayName,
    refTable: foreignKey.ref_table,
    data: [],
  };
  const getForeignKeyOptionsThrottled = useMemo(
    () =>
      debounce((value: string) => {
        return (
          displayName &&
          columnFkOpts.current &&
          getFkOptions({ ...columnFkOpts.current, displayName }, value)
        );
      }, 500),
    [getFkOptions, displayName]
  );
  const onSearchValueChange = (value: string | undefined | null) => {
    if (columnFkOpts && displayName && value) {
      getForeignKeyOptionsThrottled(value);
    }
  };
  // update local storage
  useEffect(() => {
    if (foreignKey) {
      upsertLocalStorageObject(DISPLAY_NAME_PREF_KEY, {
        [foreignKey.constraint_name]: displayName,
      });
    }
  }, [displayName, foreignKey]);
  const options = columnFkOpts.current
    ? columnFkOpts.current.data.map(row => ({
        label: `${row[columnFkOpts.current!.displayName]} (${
          row[columnFkOpts.current!.to]
        })`,
        value: row[columnFkOpts.current!.to],
      }))
    : [];
  const getValue = () => {
    if (!selectedOption) {
      return prevValue ? createOpt(prevValue) : undefined;
    }
    return selectedOption;
  };
  return (
    <>
      <span className={wrapperClassName}>
        <SearchableSelect
          {...standardInputProps}
          isCreatable
          options={options}
          onChange={onFkValueChange}
          value={getValue()}
          bsClass={styles.insertBox}
          styleOverrides={searchableSelectStyles}
          onSearchValueChange={onSearchValueChange}
          filterOption="fulltext"
          // Treating last search value the same was as selected option,
          // so that user don't have to click in the dropdown, they can just leave the input
          createNewOption={createOpt}
          // onMenuClose={onMenuClose}
          placeholder={placeholder}
        />
      </span>
      {children}
      <div className={styles.displayNameSelect}>
        <DisplayNameSelect
          displayName={displayName}
          options={displayNames}
          onChange={setDisplayName}
        />
      </div>
    </>
  );
};
