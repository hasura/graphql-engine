import React from 'react';
import { FaEdit } from 'react-icons/fa';
import styles from '../Common.module.scss';
import { PrimaryDBInfo } from './PrimaryDBInfo';
import { TryOperation } from './TryOperation';
import { CreateRestEndpoint } from './CreateRestEndpoints';

class Heading extends React.Component {
  state = {
    text: this.props.currentValue,
    isEditting: false,
  };

  handleTextChange = e => {
    this.setState({ text: e.target.value });
  };

  toggleEditting = () => {
    this.setState({ isEditting: !this.state.isEditting });
  };

  handleKeyPress = e => {
    if (this.state.isEditting) {
      if (e.charCode === 13) {
        this.save();
      }
    }
  };

  save = () => {
    if (this.props.loading) {
      return;
    }
    this.props.save(this.state.text);
  };

  render = () => {
    const {
      editable,
      currentValue,
      save,
      loading,
      property,
      table,
      dispatch,
      source,
    } = this.props;
    const { text, isEditting } = this.state;

    if (!editable) {
      return (
        <>
          <div className={`${styles.editable_heading_text} pb-2`}>
            <h2>{currentValue}</h2>
            <div className="text-base font-normal flex gap-2">
              <TryOperation table={table} dispatch={dispatch} source={source} />
              <CreateRestEndpoint tableName={table.table_name} />
            </div>
          </div>
          <div className="pb-5">
            <PrimaryDBInfo source={source} />
          </div>
        </>
      );
    }

    if (!save) {
      console.warn('In EditableHeading, please provide a prop save');
    }

    if (!isEditting) {
      return (
        <>
          <div className={styles.editable_heading_text}>
            <h2>{currentValue}</h2>
            <div className="text-base font-normal flex gap-2">
              <TryOperation table={table} dispatch={dispatch} source={source} />
              <CreateRestEndpoint tableName={table.table_name} />
            </div>
            <div
              onClick={this.toggleEditting}
              className={styles.editable_heading_action}
              data-test={`heading-edit-${property}`}
            >
              <FaEdit />
            </div>
          </div>
          <div className="pb-5">
            <PrimaryDBInfo source={source} />
          </div>
        </>
      );
    }

    return (
      <div className={styles.editable_heading_textbox}>
        <input
          onChange={this.handleTextChange}
          className={`${styles.add_pad_left} form-control`}
          type="text"
          onKeyPress={this.handleKeyPress}
          value={text}
          data-test={`heading-edit-${property}-input`}
        />
        <div className={styles.editable_heading_action}>
          <div
            className={styles.editable_heading_action_item}
            onClick={this.save}
            data-test={`heading-edit-${property}-save`}
          >
            {loading ? 'Saving...' : 'Save'}
          </div>
          <div
            className={styles.editable_heading_action_item}
            onClick={this.toggleEditting}
            data-test={`heading-edit-${property}-cancel`}
          >
            Cancel
          </div>
        </div>
      </div>
    );
  };
}

export default Heading;
