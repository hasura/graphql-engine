import React from 'react';
import Button from '../../Button/Button';
import styles from '../../Common.scss';

class Editor extends React.Component {
  state = {
    isEditing: this.props.toggled || false,
  };

  componentWillReceiveProps(nextProps) {
    if (nextProps.toggled !== this.state.isEditing) {
      this.setState({
        isEditing: nextProps.toggled,
      });
    }
  }

  toggleEditor = () => {
    if (this.props.expandCallback && !this.state.isEditing) {
      this.props.expandCallback();
    } else if (this.props.collapseCallback && this.state.isEditing) {
      this.props.collapseCallback();
    }
    this.setState({
      isEditing: !this.state.isEditing,
    });
  };

  toggleButton = () => {
    if (this.props.isCollapsable === false && this.state.isEditing) {
      return null;
    }
    return (
      <Button
        className={`${styles.add_mar_small} ${styles.add_mar_bottom_mid}`}
        color="white"
        size="xs"
        data-test={`${this.props.service}-${
          this.state.isEditing ? 'close' : 'edit'
        }-${this.props.property}`}
        onClick={this.toggleEditor}
      >
        {this.state.isEditing
          ? this.props.collapseButtonText || 'Close'
          : this.props.expandButtonText || 'Edit'}
      </Button>
    );
  };

  saveButton = saveFunc => {
    const { service, property, ongoingRequest, saveButtonColor } = this.props;
    const isProcessing = ongoingRequest === property;
    const saveWithToggle = () => saveFunc(this.toggleEditor);
    return (
      <Button
        type="submit"
        color={saveButtonColor || 'yellow'}
        size="sm"
        className={styles.add_mar_right}
        onClick={saveWithToggle}
        data-test={`${service}-${property}-save`}
        disabled={isProcessing}
      >
        Save
      </Button>
    );
  };

  removeButton = removeFunc => {
    const { service, property, ongoingRequest, removeButtonColor } = this.props;
    const isProcessing = ongoingRequest === property;
    const removeWithToggle = () => removeFunc(this.toggleEditor);
    return (
      <Button
        type="submit"
        color={removeButtonColor || 'red'}
        size="sm"
        onClick={removeWithToggle}
        data-test={`${service}-${property}-remove`}
        disabled={isProcessing}
      >
        Remove
      </Button>
    );
  };

  actionButtons = () => {
    const { saveFunc, removeFunc } = this.props;
    return (
      <div className={styles.editorActionButtons}>
        {saveFunc && this.saveButton(saveFunc)}
        {removeFunc && this.removeButton(removeFunc)}
      </div>
    );
  };

  render() {
    const { isEditing } = this.state;
    const {
      editorCollapsed,
      editorExpanded,
      collapsedLabel,
      expandedLabel,
    } = this.props;
    if (isEditing) {
      return (
        <div className={styles.editorExpanded}>
          <div className={styles.display_flex}>
            {this.toggleButton()}
            {expandedLabel && expandedLabel()}
          </div>
          {editorExpanded && editorExpanded()}
          {this.actionButtons()}
        </div>
      );
    }
    return (
      <div className={`${styles.editorCollapsed}`}>
        <div className={styles.display_flex}>
          {this.toggleButton()}
          {collapsedLabel && collapsedLabel()}
        </div>
        {editorCollapsed && editorCollapsed()}
      </div>
    );
  }
}

export default Editor;
