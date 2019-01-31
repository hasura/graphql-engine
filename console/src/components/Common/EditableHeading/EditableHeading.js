import React from 'react';
import styles from '../Common.scss';

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
    const { editable, currentValue, save, loading } = this.props;

    const { text, isEditting } = this.state;

    if (!editable) {
      return <h2 className={styles.heading_text}>{currentValue}</h2>;
    }

    if (!save) {
      console.warn('In EditableHeading, please provide a prop save');
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
      <div className={styles.editable_heading_textbox}>
        <input
          onChange={this.handleTextChange}
          className={`${styles.add_pad_left} form-control`}
          type="text"
          onKeyPress={this.handleKeyPress}
          value={text}
        />
        <div className={styles.editable_heading_action}>
          <div
            className={styles.editable_heading_action_item}
            onClick={this.save}
          >
            {loading ? 'Saving...' : 'Save'}
          </div>
          {!loading && (
            <div
              className={styles.editable_heading_action_item}
              onClick={this.toggleEditting}
            >
              Cancel
            </div>
          )}
        </div>
      </div>
    );
  };
}

export default Heading;
