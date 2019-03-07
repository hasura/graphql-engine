import React from 'react';
import Button from '../../Button/Button';

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
      className={`${this.props.styles.add_mar_small}
        ${this.props.styles.modifyEditButton}`}
      color="white"
      size="sm"
      data-test={`${this.props.service}-${
        this.state.isEditing ? 'close' : 'edit'
      }-${this.props.property}`}
      onClick={this.toggleEditor}
    >
      {this.state.isEditing ? 'Close' : 'Edit'}
    </Button>
  );

  saveButton = saveFunc => {
    const { service, property, ongoingRequest, styles } = this.props;
    const isProcessing = ongoingRequest === property;
    return (
      <Button
        type="submit"
        className={styles.modifySaveButton}
        color="yellow"
        size="sm"
        onClick={saveFunc}
        data-test={`${service}-${property}-save`}
        disabled={isProcessing}
      >
        Save
      </Button>
    );
  };

  removeButton = removeFunc => {
    const { service, property, ongoingRequest, styles } = this.props;
    const isProcessing = ongoingRequest === property;
    return (
      <Button
        type="submit"
        className={styles.modifySaveButton}
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

  render() {
    const { isEditing } = this.state;
    const { editorCollapsed, editorExpanded } = this.props;

    return isEditing
      ? editorExpanded(this.toggleButton, this.saveButton, this.removeButton)
      : editorCollapsed(this.toggleButton);
  }
}

export default Editor;
