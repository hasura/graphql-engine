import React from 'react';
import Button from '../../Button/Button';

class Editor extends React.Component {
  static getDerivedStateFromProps(nextProps, state) {
    if (nextProps.toggled !== state.isEditing) {
      if (nextProps.toggled !== undefined) {
        return {
          isEditing: nextProps.toggled,
        };
      }
    }

    return null;
  }

  constructor(props) {
    super(props);
    this.state = {
      isEditing: props.toggled || false,
    };
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
    const {
      readOnlyMode = false,
      isCollapsable,
      service,
      property,
      collapseButtonText,
      expandButtonText,
    } = this.props;

    const { isEditing } = this.state;
    if (isCollapsable === false && isEditing) {
      return null;
    }

    return (
      <Button
        color="white"
        size="xs"
        className="mr-sm"
        data-test={`${service}-${isEditing ? 'close' : 'edit'}-${property}`}
        onClick={this.toggleEditor}
        disabled={readOnlyMode}
      >
        {isEditing ? collapseButtonText || 'Close' : expandButtonText || 'Edit'}
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
        className="mr-sm"
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
      <div className="flex items-center mt-sm">
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
      readOnlyMode = false,
    } = this.props;

    let editorClass;
    let editorLabel;
    let editorContent;
    let actionButtons;

    if (isEditing) {
      editorClass = 'block rounded bg-white border border-gray-300 p-md mb-sm';
      editorLabel = expandedLabel && expandedLabel();
      actionButtons = this.actionButtons();

      if (editorExpanded) {
        editorContent = <div>{editorExpanded()}</div>;
      }
    } else {
      editorClass = 'block';
      editorLabel = collapsedLabel && collapsedLabel();

      if (editorCollapsed) {
        editorContent = <div>{editorCollapsed()}</div>;
      }
    }

    return (
      <div className={`space-y-md ${editorClass}`}>
        <div className="mb-sm flex items-center">
          {this.toggleButton()}
          {editorLabel}
        </div>
        {editorContent}
        {!readOnlyMode && actionButtons}
      </div>
    );
  }
}

export default Editor;
