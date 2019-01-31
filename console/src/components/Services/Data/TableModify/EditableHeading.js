import React from 'react';
import styles from '../../../Common/Common.scss';

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

  save = () => {
    const { callback } = this.props;
    callback(this.state.text);
    return;
    // dispatch({ type: saveAction, data: this.state.text }).then(() => {});
  };

  render = () => {
    const { editable, currentValue, saveAction } = this.props;

    const { text, isEditting } = this.state;

    if (!editable) {
      return <h2 className={styles.heading_text}>{currentValue}</h2>;
    }

    if (!saveAction) {
      console.warn('In EditableHeading, please provide a prop saveAction');
    }

    if (!isEditting) {
      return (
        <div className={styles.editable_heading_text}>
          <h2>{currentValue}</h2>
          <div
            onClick={this.toggleEditting}
            className={styles.editable_heading_action}
          >
            <i className="fa fa-edit" />
          </div>
        </div>
      );
    }

    return (
      <div className={styles.editable_heading_text}>
        <input
          onChange={this.handleTextChange}
          className={`${styles.add_pad_left} form-control`}
          type="text"
          value={text}
        />
        <div className={styles.editable_heading_action}>
          <div
            className={styles.editable_heading_action_item}
            onClick={this.save}
          >
            Save
          </div>
          <div
            className={styles.editable_heading_action_item}
            onClick={this.toggleEditting}
          >
            Cancel
          </div>
        </div>
      </div>
    );
  };
}

export default Heading;
