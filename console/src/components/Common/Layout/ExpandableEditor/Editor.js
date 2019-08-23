import React from 'react';
import Button from '../../Button/Button';
import styles from './Editor.scss';

class Editor extends React.Component {
  state = {
    isEditing: this.props.toggled || false,
  };

  componentWillReceiveProps(nextProps) {
    if (nextProps.toggled !== this.state.isEditing) {
      if (nextProps.toggled !== undefined) {
        this.setState({
          isEditing: nextProps.toggled,
        });
      }
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
        className={`${styles.add_mar_small}`}
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
        {this.props.saveButtonText || 'Save'}
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
        {this.props.removeButtonText || 'Remove'}
      </Button>
    );
  };

  actionButtons = () => {
    const { saveFunc, removeFunc } = this.props;
    return (
      <div>
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

    let editorClass;
    let editorLabel;
    let editorContent;
    let actionButtons;

    if (isEditing) {
      editorClass = styles.editorExpanded;
      editorLabel = expandedLabel && expandedLabel();
      actionButtons = this.actionButtons();

      if (editorExpanded) {
        editorContent = (
          <div className={styles.editorContent}>{editorExpanded()}</div>
        );
      }
    } else {
      editorClass = styles.editorCollapsed;
      editorLabel = collapsedLabel && collapsedLabel();

      if (editorCollapsed) {
        editorContent = (
          <div className={styles.editorContent}>{editorCollapsed()}</div>
        );
      }
    }

    return (
      <div className={editorClass}>
        <div className={styles.display_flex + ' ' + styles.add_mar_bottom_mid}>
          {this.toggleButton()}
          {editorLabel}
        </div>
        {editorContent}
        <div className={styles.add_mar_top_small}>{actionButtons}</div>
      </div>
    );
  }
}

export default Editor;
