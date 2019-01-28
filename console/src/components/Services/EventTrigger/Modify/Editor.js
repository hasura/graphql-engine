import React from 'react';
import Button from '../../Layout/Button/Button';

class Editor extends React.Component {
  state = {
    isEditing: false,
  };

  toggleEditor = () => {
    if (this.props.toggleCallback && !this.state.isEditing) {
      this.props.toggleCallback();
    }
    this.setState({
      ...this.state,
      isEditing: !this.state.isEditing,
    });
  };

  toggleButton = () => (
    <Button
      className={`${this.props.styles.add_mar_small}
        ${this.props.styles.modifyEditButton}`}
      color="white"
      size="sm"
      data-test={`${this.state.isEditing ? 'close' : 'edit'}-${
        this.props.name
      }`}
      onClick={this.toggleEditor}
    >
      {this.state.isEditing ? 'Close' : 'Edit'}
    </Button>
  );

  saveButton = saveFunc => {
    const { name, property, ongoingRequest, styles } = this.props;
    const isSaving = ongoingRequest === property;
    return (
      <Button
        type="submit"
        className={styles.modifySaveButton}
        color="yellow"
        size="sm"
        onClick={saveFunc}
        data-test={`modify-trigger-${name}-save`}
        disabled={isSaving}
      >
        {isSaving ? 'Saving ...' : 'Save'}
      </Button>
    );
  };

  render() {
    const { isEditing } = this.state;
    const { editorCollapsed, editorExpanded } = this.props;

    return isEditing
      ? editorExpanded(this.toggleButton, this.saveButton)
      : editorCollapsed(this.toggleButton);
  }
}

export default Editor;
