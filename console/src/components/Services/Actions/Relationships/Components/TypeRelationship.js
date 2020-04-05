import React from 'react';
import styles from '../../Actions.scss';
import { updateSchemaInfo } from '../../../Data/DataActions';
import { isEmpty, getLastArrayElement } from '../../../../Common/utils/jsUtils';
import ExpandableEditor from '../../../../Common/Layout/ExpandableEditor/Editor';
import {
  getSchemaName,
  getSchemaTables,
  getTableColumnNames,
  getTableName,
  getTrackedTables,
} from '../../../../Common/utils/pgUtils';
import {
  parseCustomTypeRelationship,
  getRelValidationError,
  getRelDef,
} from '../utils';
import {
  defaultRelConfig,
  defaultRelFieldMapping,
} from '../../Common/stateDefaults';
import { addActionRel, removeActionRel } from '../../ServerIO';
import { showErrorNotification } from '../../../Common/Notification';
import tableStyles from '../../../../Common/TableCommon/TableStyles.scss';
import { Icon } from '../../../../UIKit/atoms';

const RelationshipEditor = ({
  objectType,
  schemaList,
  allTables,
  dispatch,
  existingRelConfig,
  stateCallback,
}) => {
  const [relConfig, setRelConfig] = React.useState(
    existingRelConfig || defaultRelConfig
  );

  // if it is an existing relationship, fetch the pg schema metadata
  React.useEffect(() => {
    if (existingRelConfig) {
      dispatch(updateSchemaInfo({ schemas: [existingRelConfig.refSchema] }));
    }
  }, []);

  // hoist the state to parent whenever there's a change
  React.useEffect(() => {
    if (stateCallback) {
      stateCallback(relConfig);
    }
  }, [relConfig]);

  const { name, type, refSchema, refTable, fieldMapping } = relConfig;

  // relname on change
  const setRelName = e => {
    const relName = e.target.value;
    setRelConfig(rc => ({
      ...rc,
      name: relName,
    }));
  };

  // reltype on change
  const setRelType = e => {
    const relType = e.target.value;
    setRelConfig(rc => ({
      ...rc,
      type: relType,
    }));
  };

  // ref schema on change
  const setRelRefSchema = e => {
    const selectedSchema = e.target.value;
    setRelConfig(rc => ({
      ...rc,
      refSchema: selectedSchema,
      refTable: '',
      fieldMapping: [defaultRelFieldMapping],
    }));
    dispatch(updateSchemaInfo({ schemas: [selectedSchema] }));
  };

  // ref table on change
  const setRelRefTable = e => {
    const refTable_ = e.target.value;
    setRelConfig(rc => ({
      ...rc,
      refTable: refTable_,
      fieldMapping: [defaultRelFieldMapping],
    }));
  };

  // field mappings on change
  const setFieldMappings = f_ => {
    const f = JSON.parse(JSON.stringify(f_));
    const lastFieldMapping = getLastArrayElement(f);
    if (!isEmpty(f) && lastFieldMapping) {
      if (!!lastFieldMapping.column && !!lastFieldMapping.field) {
        f.push(defaultRelFieldMapping);
      }
    }
    setRelConfig(rc => ({
      ...rc,
      fieldMapping: f,
    }));
  };

  // get the tracked tables in a schema
  const refTables = getTrackedTables(
    getSchemaTables(allTables, refSchema)
  ).reduce((all, trackedTable) => {
    return {
      ...all,
      [getTableName(trackedTable)]: getTableColumnNames(trackedTable),
    };
  }, {});

  // rel name input
  const getRelNameInput = () => {
    /*
      There is no neat solution to renaming a relationship.
      This is because name is the only unique identifier of a relationship.
      TODO FIXME
    */
    const isDisabled = !!existingRelConfig;
    const relNameInputTitle = isDisabled
      ? 'A relationship cannot be renamed. Please drop and re-create if you really must.'
      : undefined;
    return (
      <div className={`${styles.add_mar_bottom}`}>
        <div className={`${styles.add_mar_bottom_mid}`}>
          <b>Relationship Name:</b>
        </div>
        <input
          onChange={setRelName}
          type="text"
          className={`${styles.select} form-control ${styles.add_pad_left}`}
          placeholder="Enter relationship name"
          data-test="rel-name"
          title={relNameInputTitle}
          value={name}
        />
      </div>
    );
  };

  // rel type select
  const getRelTypeSelect = () => {
    return (
      <div className={`${styles.add_mar_bottom}`}>
        <div className={`${styles.add_mar_bottom_mid}`}>
          <b>Relationship Type:</b>
        </div>
        <select
          value={type}
          className={`${styles.select} form-control ${styles.add_pad_left}`}
          data-test={'manual-relationship-type'}
          onChange={setRelType}
        >
          {type === '' && (
            <option value={''} disabled>
              {'-- relationship type --'}
            </option>
          )}
          <option key="object" value="object">
            Object Relationship
          </option>
          <option key="array" value="array">
            Array Relationship
          </option>
        </select>
      </div>
    );
  };

  // ref schema select
  const getRefSchemaSelect = () => {
    const orderedSchemaList = schemaList.map(s => getSchemaName(s)).sort();
    return (
      <div className={`${styles.add_mar_bottom}`}>
        <div className={`${styles.add_mar_bottom_mid}`}>
          <b>Reference Schema:</b>
        </div>
        <select
          value={refSchema}
          className={`${styles.select} form-control ${styles.add_pad_left}`}
          data-test={'manual-relationship-ref-schema'}
          onChange={setRelRefSchema}
          disabled={!name}
        >
          {// default unselected option
            refSchema === '' && (
              <option value={''} disabled>
                {'-- reference schema --'}
              </option>
            )}
          {// all reference schema options
            orderedSchemaList.map((rs, j) => (
              <option key={j} value={rs}>
                {rs}
              </option>
            ))}
        </select>
      </div>
    );
  };

  // ref table select
  const getRefTableSelect = () => {
    return (
      <div className={`${styles.add_mar_bottom}`}>
        <div className={`${styles.add_mar_bottom_mid}`}>
          <b>Reference Table:</b>
        </div>
        <select
          value={refTable}
          className={`${styles.select} form-control ${styles.add_pad_left}`}
          data-test={'manual-relationship-ref-table'}
          onChange={setRelRefTable}
          disabled={!refSchema}
        >
          {refTable === '' && (
            <option value={''} disabled>
              {'-- reference table --'}
            </option>
          )}
          {Object.keys(refTables)
            .sort()
            .map((rt, j) => (
              <option key={j} value={rt}>
                {rt}
              </option>
            ))}
        </select>
      </div>
    );
  };

  // field mapping array builder
  const getRelFieldMappings = () => {
    return (
      <div className={`${styles.add_mar_bottom}`}>
        <div className={`row ${styles.add_mar_bottom_mid}`}>
          <div className={`col-sm-4 ${styles.add_mar_right}`}>
            <b>From:</b>
          </div>
          <div className={`col-sm-4 ${styles.add_mar_right}`}>
            <b>To:</b>
          </div>
        </div>
        {fieldMapping.map((fieldMap, i) => {
          const setColumn = e => {
            const selectedCol = e.target.value;
            const newFM = JSON.parse(JSON.stringify(fieldMapping));
            newFM[i] = {
              ...fieldMap,
              column: selectedCol,
            };
            setFieldMappings(newFM);
          };

          const setField = e => {
            const selectedField = e.target.value;
            const newFM = JSON.parse(JSON.stringify(fieldMapping));
            newFM[i] = {
              ...fieldMap,
              field: selectedField,
            };
            setFieldMappings(newFM);
          };

          const field = fieldMap.field;
          const refColumn = fieldMap.column;

          const removeField = () => {
            setFieldMappings([
              ...fieldMapping.slice(0, i),
              ...fieldMapping.slice(i + 1),
            ]);
          };

          let removeIcon;
          if (i + 1 === fieldMapping.length) {
            removeIcon = null;
          } else {
            removeIcon = (
              <Icon
                type="close"
                onClick={removeField}
                ml="10px"
                size={15}
                pointer
              />
            );
          }

          const fields = objectType.fields.map(f => f.name);

          const selectTitle = !refTable
            ? 'Please select the reference table'
            : undefined;

          return (
            <div
              className={`row ${styles.add_mar_bottom_mid} ${styles.display_flex}`}
              key={`fk-col-${i}`}
            >
              <div className={`col-sm-4 ${styles.add_mar_right}`}>
                <select
                  className={`form-control ${styles.select} ${styles.wd100Percent}`}
                  value={field}
                  onChange={setField}
                  data-test={`manual-relationship-lcol-${i}`}
                  disabled={!refSchema || !refTable}
                  title={selectTitle}
                >
                  {field === '' && (
                    <option value="" disabled>
                      {'-- field --'}
                    </option>
                  )}
                  {fields.map(f => {
                    return (
                      <option key={f} value={f}>
                        {f}
                      </option>
                    );
                  })}
                </select>
              </div>
              <div className={'col-sm-4'}>
                <select
                  className={`form-control ${styles.select} ${styles.wd100Percent}`}
                  value={refColumn}
                  onChange={setColumn}
                  disabled={!refTable}
                  title={selectTitle}
                  data-test={`manual-relationship-rcol-${i}`}
                >
                  {refColumn === '' && (
                    <option value="" disabled>
                      {'-- ref_column --'}
                    </option>
                  )}
                  {refTables[refTable] &&
                    refTables[refTable].map(rcOpt => {
                      return (
                        <option key={rcOpt} value={rcOpt}>
                          {rcOpt}
                        </option>
                      );
                    })}
                </select>
              </div>
              <div>{removeIcon}</div>
            </div>
          );
        })}
      </div>
    );
  };

  return (
    <div className="form-group">
      {getRelTypeSelect()}
      {getRelNameInput()}
      {getRefSchemaSelect()}
      {getRefTableSelect()}
      {getRelFieldMappings()}
    </div>
  );
};

const RelEditor = props => {
  const { dispatch, relConfig, objectType, isNew } = props;

  const [relConfigState, setRelConfigState] = React.useState(null);

  // in case of existing relationship, show rel definition as label
  const collapsedLabel = () => {
    if (!relConfig) return null;
    return (
      <div>
        <b>{relConfig.name}</b>
        <div className={tableStyles.relationshipTopPadding}>
          {getRelDef({ ...relConfig, typename: objectType.name })}
        </div>
      </div>
    );
  };

  // the relationship editor
  const expandedContent = () => {
    let existingRelConfig = null;
    // pass the init state to the editor if it is an existing relationship
    if (relConfig) {
      existingRelConfig = parseCustomTypeRelationship(relConfig);
    }
    return (
      <RelationshipEditor
        {...props}
        existingRelConfig={existingRelConfig}
        stateCallback={setRelConfigState}
      />
    );
  };

  // reset the state whenever the editor is collapsed
  const collapseCallback = () => {
    setRelConfigState(null);
  };

  // function to save the relationship
  const saveFunc = toggle => {
    const validationError = getRelValidationError(relConfigState);
    if (validationError) {
      return dispatch(
        showErrorNotification('Cannot create relationship', validationError)
      );
    }
    dispatch(
      addActionRel(
        { ...relConfigState, typename: objectType.name },
        toggle,
        isNew ? null : relConfig
      )
    );
  };

  // function to remove the relationship
  let removeFunc;
  if (!isNew) {
    removeFunc = toggle => {
      dispatch(removeActionRel(relConfig.name, objectType.name, toggle));
    };
  }

  const expandButtonText = isNew ? 'Add a relationship' : 'Edit';
  const collapseButtonText = isNew ? 'Cancel' : 'Close';

  return (
    <ExpandableEditor
      collapseCallback={collapseCallback}
      editorExpanded={expandedContent}
      expandButtonText={expandButtonText}
      collapsedLabel={collapsedLabel}
      property={'relationship'}
      service="actions"
      saveFunc={saveFunc}
      removeFunc={removeFunc}
      expandButtonText={expandButtonText}
      collapseButtonText={collapseButtonText}
    />
  );
};

export default RelEditor;
