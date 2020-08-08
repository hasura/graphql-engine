import React from 'react';
import Spinner from '../../../../Common/Spinner/Spinner';
import { connect } from 'react-redux';
import { isInputObjectType, isObjectType, isEnumType } from 'graphql';
import { deriveExistingType } from '../utils';
import Tooltip from './Tooltip';
import styles from './Styles.scss';
import { useIntrospectionSchema } from '../../../../Common/utils/graphqlUtils';

const CloneType = ({ headers, toggleModal, handleClonedTypes, dispatch }) => {
  const [prefix, setPrefix] = React.useState('_');
  const prefixOnChange = e => setPrefix(e.target.value);

  const { schema, loading, error, introspect } = useIntrospectionSchema(
    headers,
    dispatch
  );

  if (loading) return <Spinner />;

  if (error) {
    return (
      <div>
        Error introspecting schema.&nbsp;
        <a onClick={introspect}>Try again</a>
      </div>
    );
  }

  const cloneableTypes = Object.keys(schema._typeMap)
    .filter(t => {
      if (t.startsWith('__')) return false;
      return (
        isInputObjectType(schema._typeMap[t]) ||
        isObjectType(schema._typeMap[t]) ||
        isEnumType(schema._typeMap[t])
      );
    })
    .sort((t1, t2) => {
      const _t1 = t1.toLowerCase();
      const _t2 = t2.toLowerCase();
      if (_t1 > _t2) return 1;
      if (_t1 < _t2) return -1;
      return 0;
    });

  const onSelection = e => {
    const selectedType = e.target.value;
    if (selectedType === '') return;
    const newTypes = deriveExistingType(selectedType, schema._typeMap, prefix);
    handleClonedTypes(newTypes);
    toggleModal();
  };

  const dropdownTitle = prefix ? null : 'Please provide a prefix first.';

  const prefixTooltipText =
    'Prefix is required so that the type you are cloning does not collide with the existing type in Hasura.';

  return (
    <div>
      <div
        className={`row ${styles.add_mar_bottom_mid} ${styles.display_flex}`}
      >
        <div className={'col-md-3'}>
          Prefix <Tooltip text={prefixTooltipText} id="clone-prefix" />
        </div>
        <input
          type="text"
          value={prefix}
          onChange={prefixOnChange}
          className={`form-control col-md-3 ${styles.inputWidth}`}
        />
      </div>
      <div
        className={`row ${styles.add_mar_bottom_mid} ${styles.display_flex}`}
      >
        <div className="col-md-3"> Type to clone</div>
        <select
          value=""
          className={`form-control col-md-3 ${styles.inputWidth}`}
          onChange={onSelection}
          disabled={prefix === ''}
          title={dropdownTitle}
        >
          <option value="">---select an existing type---</option>
          {cloneableTypes.map(t => {
            return (
              <option value={t} key={t}>
                {t}
              </option>
            );
          })}
        </select>
      </div>
    </div>
  );
};

const mapStateToprops = state => {
  return {
    headers: state.tables.dataHeaders,
  };
};

export default connect(mapStateToprops)(CloneType);
