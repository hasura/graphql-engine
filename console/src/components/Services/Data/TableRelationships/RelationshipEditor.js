/* eslint-disable jsx-a11y/no-autofocus */
import React from 'react';
import { getRelDef } from './utils';
import Button from '../../../Common/Button/Button';
import { deleteRelMigrate, saveRenameRelationship } from './Actions';
import { showErrorNotification } from '../Notification';
import gqlPattern, { gqlRelErrorNotif } from '../Common/GraphQLValidation';
import styles from '../TableModify/ModifyTable.scss';

class RelationshipEditor extends React.Component {
  state = {
    isEditting: false,
    text: this.props.relName,
  };

  handleTextChange = e => {
    this.setState({
      text: e.target.value,
    });
  };

  toggleEditor = () => {
    this.setState({
      isEditting: !this.state.isEditting,
    });
  };

  handleKeyPress = e => {
    if (this.state.isEditting) {
      if (e.charCode === 13) {
        this.save();
      }
    }
  };

  save = () => {
    const { tableName, relName, dispatch } = this.props;
    const { text } = this.state;
    if (text === relName) {
      return dispatch(
        showErrorNotification(
          'Renaming relationship failed',
          `The relationship name is already ${relName}`
        )
      );
    }
    if (!gqlPattern.test(text)) {
      return dispatch(
        showErrorNotification(
          gqlRelErrorNotif[4],
          gqlRelErrorNotif[1],
          gqlRelErrorNotif[2],
          gqlRelErrorNotif[3]
        )
      );
    }
    dispatch(
      saveRenameRelationship(relName, text, tableName, this.toggleEditor)
    );
  };

  render() {
    const {
      dispatch,
      tableName,
      relName,
      relConfig,
      isObjRel,
      allowRename,
    } = this.props;

    const { text, isEditting } = this.state;
    const { lcol, rtable, rcol } = relConfig;

    const tableStyles = require('../../../Common/TableCommon/TableStyles.scss');

    const onDelete = e => {
      e.preventDefault();
      const isOk = confirm('Are you sure?');
      if (isOk) {
        dispatch(
          deleteRelMigrate(tableName, relName, lcol, rtable, rcol, isObjRel)
        );
      }
    };
    const collapsed = () => (
      <div>
        <Button
          color={allowRename ? 'white' : 'red'}
          size={allowRename ? 'xs' : 'sm'}
          onClick={allowRename ? this.toggleEditor : onDelete}
          data-test={
            allowRename
              ? `relationship-toggle-editor-${relName}`
              : `relationship-remove-${relName}`
          }
        >
          {allowRename ? 'Edit' : 'Remove'}
        </Button>
        &nbsp;
        <b>{relName}</b>
        <div className={tableStyles.relationshipTopPadding}>
          {getRelDef(isObjRel, lcol, rcol, tableName, rtable)}
        </div>
      </div>
    );

    const expanded = () => (
      <div className={styles.activeEdit}>
        <div className={tableStyles.add_mar_top}>
          <Button
            color="white"
            size="xs"
            onClick={this.toggleEditor}
            data-test={`relationship-toggle-editor-${relName}`}
          >
            Close
          </Button>
        </div>
        <div className={tableStyles.relationshipTopPadding}>
          <div>{getRelDef(isObjRel, lcol, rcol, tableName, rtable)}</div>
          <input
            onChange={this.handleTextChange}
            className={`form-control ${styles.add_mar_top_small}`}
            type="text"
            value={text}
            data-test={`relationship-name-input-${relName}`}
            onKeyPress={this.handleKeyPress}
            autoFocus
          />
        </div>
        <div className={tableStyles.relEditButtons}>
          <Button
            className={styles.add_mar_right}
            color="yellow"
            size="xs"
            onClick={this.save}
            data-test={`relationship-save-${relName}`}
          >
            Save
          </Button>
          <Button
            color="red"
            size="xs"
            onClick={onDelete}
            data-test={`relationship-remove-${relName}`}
          >
            Remove
          </Button>
        </div>
      </div>
    );

    return <td>{isEditting ? expanded() : collapsed()}</td>;
  }
}
export default RelationshipEditor;
