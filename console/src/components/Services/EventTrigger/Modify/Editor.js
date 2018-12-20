import React from 'react';

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
    <button
      className={`${this.props.styles.add_mar_small}
        btn btn-sm btn-default
        ${this.props.styles.modifyEditButton}`}
      data-test={`${this.state.isEditing ? 'close' : 'edit'}-${
        this.props.name
      }`}
      onClick={this.toggleEditor}
    >
      {this.state.isEditing ? 'Close' : 'Edit'}
    </button>
  );

  saveButton = saveFunc => {
    const { name, property, ongoingRequest, styles } = this.props;
    const isSaving = ongoingRequest === property;
    return (
      <button
        type="submit"
        className={`
          btn
          ${styles.yellow_button}
          ${
            isSaving ? styles.modifySaveButtonDisabled : styles.modifySaveButton
          }`}
        onClick={saveFunc}
        data-test={`modify-trigger-${name}-save`}
        disabled={isSaving}
      >
        {isSaving ? 'Saving ...' : 'Save'}
      </button>
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
