import React from 'react';
// import { getRelDef } from './utils';
import Button from '../../../../Common/Button/Button';
// import { deleteRelMigrate, saveRenameRelationship } from '../reducer';
import { showErrorNotification } from '../../../Common/Notification';
import gqlPattern, {
  gqlRelErrorNotif,
} from '../../../Data/Common/GraphQLValidation';
import GqlCompatibilityWarning from '../../../../Common/GqlCompatibilityWarning/GqlCompatibilityWarning';

import styles from '../../Actions.scss';
import tableStyles from '../../../../Common/TableCommon/TableStyles.scss';

// import { getConfirmation } from '../../../../Common/utils/jsUtils';

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
    // dispatch(
    //   saveRenameRelationship(
    //     relConfig.relName,
    //     text,
    //     relConfig.lTable,
    //     this.toggleEditor
    //   )
    // );
  };

  render() {
    const { relConfig } = this.props;
    const { text, isEditting } = this.state;

    const { name } = relConfig;

    const gqlCompatibilityWarning = !gqlPattern.test(name) ? (
      <span className={styles.add_mar_left_small}>
        <GqlCompatibilityWarning />
      </span>
    ) : null;

    const onDelete = e => {
      e.preventDefault();

      // const confirmMessage = `This will delete the relationship "${relName}" from this table`;
      // const isOk = getConfirmation(confirmMessage);
      // if (isOk) {
      //   dispatch(deleteRelMigrate(relConfig));
      // }
    };

    const getRelDef = relMeta => {
      const lcol =
        Object.keys(relMeta.field_mapping).length > 1
          ? '( ' + Object.keys(relMeta.field_mapping).join(', ') + ' )'
          : Object.keys(relMeta.field_mapping)[0];
      const rcol =
        Object.values(relMeta.field_mapping).length > 1
          ? '( ' + Object.values(relMeta.field_mapping).join(', ') + ' )'
          : Object.values(relMeta.field_mapping)[0];

      return (
        <span>
          {lcol} &nbsp;&rarr;&nbsp;
          {relMeta.remote_table} . {rcol}
        </span>
      );
    };

    const collapsed = () => (
      <div>
        {/*<Button*/}
        {/*color={'white'}*/}
        {/*size={'xs'}*/}
        {/*onClick={this.toggleEditor}*/}
        {/*data-test={`relationship-toggle-editor-${name}`}*/}
        {/*>*/}
        {/*Edit*/}
        {/*</Button>*/}
        {/*&nbsp;*/}
        <b>{name}</b> {gqlCompatibilityWarning}
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
            data-test={`relationship-toggle-editor-${name}`}
          >
            Close
          </Button>
        </div>
        <div className={tableStyles.relationshipTopPadding}>
          {/*<div>{getRelDef(relConfig)}</div>*/}
          <input
            onChange={this.handleTextChange}
            className={`form-control ${styles.add_mar_top_small}`}
            type="text"
            value={text}
            data-test={`relationship-name-input-${name}`}
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
            data-test={`relationship-save-${name}`}
          >
            Save
          </Button>
          <Button
            color="red"
            size="xs"
            onClick={onDelete}
            data-test={`relationship-remove-${name}`}
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
