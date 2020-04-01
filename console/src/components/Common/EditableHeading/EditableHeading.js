import React from 'react';

import { Icon, Heading } from '../../UIKit/atoms';
import styles from '../Common.scss';

class EditableHeading extends React.Component {
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
    const { editable, currentValue, save, loading, property } = this.props;

    const { text, isEditting } = this.state;

    if (!editable) {
      return (
        <Heading as="h2" fontSize="18px" pb="20px">
          {currentValue}
        </Heading>
      );
    }

    if (!save) {
      console.warn('In EditableHeading, please provide a prop save');
    }

    if (!isEditting) {
      return (
        <div className={styles.editable_heading_text}>
          <Heading as="h2" fontSize="18px" mr="sm">
            {currentValue}
          </Heading>
          <div
            onClick={this.toggleEditting}
            className={styles.editable_heading_action}
            data-test={`heading-edit-${property}`}
          >
            <Icon type="edit" pointer />
          </div>
        </div>
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

export default EditableHeading;
