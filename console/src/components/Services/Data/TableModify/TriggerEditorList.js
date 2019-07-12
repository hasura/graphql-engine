import React from 'react';

import ExpandableEditor from '../../../Common/Layout/ExpandableEditor/Editor';

import styles from './ModifyTable.scss';
import { deleteTrigger } from './ModifyActions';

const TriggerEditorList = ({ tableSchema, dispatch }) => {
  const triggers = tableSchema.triggers;

  if (!triggers.length) {
    return <div>No triggers</div>;
  }

  return triggers.map((trigger, i) => {
    const triggerName = trigger.trigger_name;

    const onDelete = () => {
      const isOk = confirm('Are you sure you want to delete?');
      if (isOk) {
        dispatch(deleteTrigger(trigger, tableSchema));
      }
    };

    const collapsedLabel = () => (
      <div>
        <div>
          <b>{triggerName}</b>&nbsp;-&nbsp;
          <i>
            {trigger.action_timing} {trigger.event_manipulation}{' '}
            {trigger.action_statement}
          </i>
        </div>
        <div className={styles.text_gray}>{trigger.comment}</div>
      </div>
    );

    const expandedLabel = () => <b>{triggerName}</b>;

    const expandedContent = () => {
      let comment;
      if (trigger.comment) {
        comment = (
          <div className={styles.text_gray + ' ' + styles.add_mar_bottom}>
            {trigger.comment}
          </div>
        );
      }

      return (
        <div>
          {comment}
          <div>
            <i>
              {trigger.action_timing} {trigger.event_manipulation}{' '}
              {trigger.action_statement}
            </i>
          </div>
        </div>
      );
    };

    return (
      <div key={`trigger-${triggerName || i}`}>
        <ExpandableEditor
          editorExpanded={expandedContent}
          expandedLabel={expandedLabel}
          collapsedLabel={collapsedLabel}
          property={`trigger-${i}`}
          service="modify-table"
          saveFunc={null}
          removeFunc={onDelete}
          isCollapsable
        />
      </div>
    );
  });
};

export default TriggerEditorList;
