import React from 'react';
import Button from '../../Button/Button';
import styles from '../../Common.scss';

class Editor extends React.Component {
  state = {
    isEditing: false,
  };

  toggleEditor = () => {
    if (this.props.toggleCallback && !this.state.isEditing) {
      this.props.toggleCallback();
    }
    this.setState({
      isEditing: !this.state.isEditing,
    });
  };

  toggleButton = () => (
    <Button
      className={`${styles.add_mar_small} ${styles.add_mar_bottom_mid}`}
      color="white"
      size="xs"
      data-test={`${this.props.service}-${
        this.state.isEditing ? 'close' : 'edit'
      }-${this.props.property}`}
      onClick={this.toggleEditor}
    >
      {this.state.isEditing ? 'Close' : 'Edit'}
    </Button>
  );

  saveButton = saveFunc => {
    const { service, property, ongoingRequest } = this.props;
    const isProcessing = ongoingRequest === property;
    return (
      <Button
        type="submit"
        color="yellow"
        size="sm"
        className={styles.add_mar_right}
        onClick={saveFunc}
        data-test={`${service}-${property}-save`}
        disabled={isProcessing}
      >
        Save
      </Button>
    );
  };

  removeButton = removeFunc => {
    const { service, property, ongoingRequest } = this.props;
    const isProcessing = ongoingRequest === property;
    return (
      <Button
        type="submit"
        color="red"
        size="sm"
        onClick={removeFunc}
        data-test={`${service}-${property}-remove`}
        disabled={isProcessing}
      >
        Remove
      </Button>
    );
  };

  actionButtons = () => (
    <div className={styles.editorActionButtons}>
      {this.props.saveFunc && this.saveButton(this.props.saveFunc)}
      {this.props.removeFunc && this.removeButton(this.props.removeFunc)}
    </div>
  );

  render() {
    const { isEditing } = this.state;
    const { editorCollapsed, editorExpanded } = this.props;
    if (isEditing) {
      return (
        <div className={styles.editorExpanded}>
          {this.toggleButton()}
          {editorExpanded()}
          {this.actionButtons()}
        </div>
      );
    }
    return (
      <div className={styles.editorCollapsed}>
        {this.toggleButton()}
        {editorCollapsed(this.toggleButton)}
      </div>
    );
  }
}

export default Editor;
