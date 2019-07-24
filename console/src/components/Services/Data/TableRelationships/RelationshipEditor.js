import React from 'react';
import { getRelDef } from './utils';
import Button from '../../../Common/Button/Button';
import { deleteRelMigrate, saveRenameRelationship } from './Actions';
import { showErrorNotification } from '../../Common/Notification';
import gqlPattern, { gqlRelErrorNotif } from '../Common/GraphQLValidation';
import GqlCompatibilityWarning from '../../../Common/GqlCompatibilityWarning/GqlCompatibilityWarning';

import styles from '../TableModify/ModifyTable.scss';
import tableStyles from '../../../Common/TableCommon/TableStyles.scss';

class RelationshipEditor extends React.Component {
  constructor(props) {
    super(props);

    this.state = {
      isEditting: false,
      text: this.props.relConfig.relName,
    };
  }

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
    const { relConfig, dispatch } = this.props;
    const { text } = this.state;
    if (text === relConfig.relName) {
      return dispatch(
        showErrorNotification(
          'Renaming relationship failed',
          `The relationship name is already ${relConfig.relName}`
        )
      );
    }
    if (!gqlPattern.test(text)) {
      return dispatch(
        showErrorNotification(
          gqlRelErrorNotif[3],
          gqlRelErrorNotif[1],
          gqlRelErrorNotif[2]
        )
      );
    }
    dispatch(
      saveRenameRelationship(
        relConfig.relName,
        text,
        relConfig.lTable,
        this.toggleEditor
      )
    );
  };

  render() {
    const { dispatch, relConfig } = this.props;
    const { text, isEditting } = this.state;
    const { relName } = relConfig;

    const gqlCompatibilityWarning = !gqlPattern.test(relName) ? (
      <span className={styles.add_mar_left_small}>
        <GqlCompatibilityWarning />
      </span>
    ) : null;

    const onDelete = e => {
      e.preventDefault();
      const isOk = confirm('Are you sure?');
      if (isOk) {
        dispatch(deleteRelMigrate(relConfig));
      }
    };
    const collapsed = () => (
      <div>
        <Button
          color={'white'}
          size={'xs'}
          onClick={this.toggleEditor}
          data-test={`relationship-toggle-editor-${relName}`}
        >
          Edit
        </Button>
        &nbsp;
        <b>{relName}</b> {gqlCompatibilityWarning}
        <div className={tableStyles.relationshipTopPadding}>
          {getRelDef(relConfig)}
        </div>
      </div>
    );

    const expanded = () => (
      <div className={styles.activeEdit}>
        <div>
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
          <div>{getRelDef(relConfig)}</div>
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
