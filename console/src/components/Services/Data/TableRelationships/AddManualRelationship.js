import React, { Component } from 'react';

import PropTypes from 'prop-types';
import {
  relNameChanged,
  relTableChange,
  REL_SET_LCOL,
  REL_SET_RCOL,
  relManualAddClicked,
  relTypeChange,
  addRelViewMigrate,
  REL_SET_MANUAL_COLUMNS,
} from './Actions';
import {
  fetchTableListBySchema,
  UPDATE_REMOTE_SCHEMA_MANUAL_REL,
  RESET_MANUAL_REL_TABLE_LIST,
} from '../DataActions';
import Button from '../../Layout/Button/Button';

class AddManualRelationship extends Component {
  constructor() {
    super();
    this.onTableChange = this.onTableChange.bind(this);
    this.onSchemaChange = this.onSchemaChange.bind(this);
    this.onRelNameChange = this.onRelNameChange.bind(this);
    this.onRelLColChange = this.onRelLColChange.bind(this);
    this.onRelRColChange = this.onRelRColChange.bind(this);
    this.onRelTypeChange = this.onRelTypeChange.bind(this);
    this.onAddRelClicked = this.onAddRelClicked.bind(this);
    this.onCloseClicked = this.onCloseClicked.bind(this);
  }
  componentDidMount() {
    /* Initializing manual relationship config with current schema and tables */
    this.props.dispatch({
      type: UPDATE_REMOTE_SCHEMA_MANUAL_REL,
      data: this.props.currentSchema,
    });
    this.props.dispatch(fetchTableListBySchema(this.props.currentSchema));
  }
  componentWillUnmount() {
    this.props.dispatch({ type: RESET_MANUAL_REL_TABLE_LIST });
  }
  onTableChange(e) {
    this.props.dispatch(relTableChange(e.target.value));
  }
  onSchemaChange(e) {
    this.props.dispatch({
      type: UPDATE_REMOTE_SCHEMA_MANUAL_REL,
      data: e.target.value,
    });
    this.props.dispatch({
      type: REL_SET_MANUAL_COLUMNS,
      data: [],
    });
    this.props.dispatch(fetchTableListBySchema(e.target.value));
  }
  onRelNameChange(e) {
    this.props.dispatch(relNameChanged(e.target.value));
  }
  onRelLColChange(e) {
    this.props.dispatch({ type: REL_SET_LCOL, lcol: e.target.value });
  }
  onRelRColChange(e) {
    this.props.dispatch({ type: REL_SET_RCOL, rcol: e.target.value });
  }
  onRelTypeChange(e) {
    if (e.target.value === 'object_rel') {
      this.props.dispatch(relTypeChange('true'));
    } else {
      this.props.dispatch(relTypeChange('false'));
    }
  }
  onAddRelClicked() {
    this.props.dispatch(addRelViewMigrate(this.props.tableName));
  }
  onCloseClicked() {
    this.props.dispatch(relManualAddClicked());
  }

  render() {
    const styles = require('../TableModify/Modify.scss');
    const {
      tableName,
      allSchemas,
      schemaList,
      manualColumns,
      manualRelInfo,
      titleInfo,
    } = this.props;

    const tableSchema = allSchemas.find(t => t.table_name === tableName);
    return (
      <div>
        <div className={styles.subheading_text}> {titleInfo} </div>
        <div className="form-group">
          <div className={`${styles.relBlockInline} ${styles.relBlockLeft}`}>
            Relationship Type
          </div>
          <div className={`${styles.relBlockInline} ${styles.relBlockRight}`}>
            <select
              className="form-control"
              onChange={this.onRelTypeChange}
              data-test="rel-type"
            >
              <option key="select_type" value="select_type">
                Select relationship type
              </option>
              <option key="object" value="object_rel">
                Object Relationship
              </option>
              <option key="array" value="array_rel">
                Array Relationship
              </option>
            </select>
          </div>
        </div>
        <div className="form-group">
          <div className={`${styles.relBlockInline} ${styles.relBlockLeft}`}>
            Relationship Name
          </div>
          <div className={`${styles.relBlockInline} ${styles.relBlockRight}`}>
            <input
              onChange={this.onRelNameChange}
              className="form-control"
              placeholder="Enter relationship name"
              data-test="rel-name"
            />
          </div>
        </div>
        <div className="form-group">
          <div className={`${styles.relBlockInline} ${styles.relBlockLeft}`}>
            Configuration
          </div>
          <select
            className={`${styles.relBlockInline} form-control ${
              styles.manual_rel_select
            }`}
            onChange={this.onRelLColChange}
            data-test="current-col"
          >
            <option key="default_column">Current Column</option>
            {tableSchema.columns.map((c, i) => (
              <option key={c + i} value={c.column_name}>
                {c.column_name}
              </option>
            ))}
          </select>
          <span> :: </span>
          <div className={styles.relBlockInline}>
            <select
              className={'form-control'}
              onChange={this.onSchemaChange}
              data-test="remote-schema"
              value={
                manualRelInfo &&
                'remoteSchema' in manualRelInfo &&
                manualRelInfo.remoteSchema
                  ? manualRelInfo.remoteSchema
                  : ''
              }
            >
              <option key="default_table">Remote Schema</option>
              {schemaList.map((s, i) => (
                <option key={i} value={s.schema_name}>
                  {s.schema_name}
                </option>
              ))}
            </select>
          </div>
          <span> . </span>
          <div className={styles.relBlockInline}>
            <select
              className={'form-control'}
              onChange={this.onTableChange}
              data-test="remote-table"
            >
              <option key="default_table">Remote Table</option>
              {manualRelInfo.tables.map((s, i) => (
                <option key={i} value={s.table_name}>
                  {s.table_name}
                </option>
              ))}
            </select>
          </div>
          <span> -> </span>
          <div className={styles.relBlockInline}>
            <select
              className={'form-control'}
              onChange={this.onRelRColChange}
              data-test="remote-table-col"
            >
              <option key="default_table_column">Remote Table Column:</option>
              {manualColumns.map((c, i) => (
                <option key={c + i} value={c.column_name}>
                  {c.column_name}
                </option>
              ))}
            </select>
          </div>
        </div>
        <Button
          color="yellow"
          size="sm"
          onClick={this.onAddRelClicked}
          data-test={this.props.dataTestVal}
        >
          Add
        </Button>
        {this.props.showClose ? (
          <Button
            className={styles.add_mar_left}
            color="white"
            size="sm"
            onClick={this.onCloseClicked}
            data-test="table-close-manual-relationship"
          >
            Close
          </Button>
        ) : null}
      </div>
    );
  }
}

AddManualRelationship.propTypes = {
  tableName: PropTypes.string.isRequired,
  titleInfo: PropTypes.string.isRequired,
  allSchemas: PropTypes.array.isRequired,
  manualColumns: PropTypes.array.isRequired,
  dispatch: PropTypes.func.isRequired,
  manualRelInfo: PropTypes.object.isRequired,
  schemaList: PropTypes.array.isRequired,
  showClose: PropTypes.bool.isRequired,
  dataTestVal: PropTypes.string.isRequired,
};

export default AddManualRelationship;
