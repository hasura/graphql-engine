import React from 'react';
import AceEditor from 'react-ace';

import ExpandableEditor from '../../../Common/Layout/ExpandableEditor/Editor';

import { deleteTrigger } from './ModifyActions';
import { getConfirmation } from '../../../Common/utils/jsUtils';
import { Text } from '../../../UIKit/atoms';
import styles from './ModifyTable.scss';

const TriggerEditorList = ({ tableSchema, dispatch }) => {
  const triggers = tableSchema.triggers;

  if (!triggers.length) {
    return <div>No triggers</div>;
  }

  // HACK
  const isEventTriggerPGTrigger = trigger => {
    return trigger.action_statement.includes('hdb_views');
  };

  return triggers.map((trigger, i) => {
    const triggerName = trigger.trigger_name;

    const onDelete = () => {
      const confirmMessage = `This will permanently delete the trigger "${triggerName}" from this table`;
      const isOk = getConfirmation(confirmMessage, true, triggerName);
      if (isOk) {
        dispatch(deleteTrigger(trigger, tableSchema));
      }
    };

    const collapsedLabel = () => (
      <div>
        <div>
          <Text fontWeight="bold" mr="sm">
            {triggerName}
          </Text>
          <i>
            {trigger.action_timing} {trigger.event_manipulation}
          </i>
        </div>
        <div className={styles.text_gray}>{trigger.comment}</div>
      </div>
    );

    const expandedLabel = () => (
      <Text fontWeight="bold" mr="sm">
        {triggerName}
      </Text>
    );

    const expandedContent = () => {
      let commentText;
      if (trigger.comment) {
        commentText = trigger.comment;
      } else if (isEventTriggerPGTrigger(trigger)) {
        commentText = 'This is a custom trigger generated for an Event trigger';
      }

      let comment;

      if (commentText) {
        comment = (
          <Text mt="20px" color="#767e96">
            {commentText}
          </Text>
        );
      }

      return (
        <div>
          <div>
            <i>
              {trigger.action_timing} {trigger.event_manipulation}, FOR EACH{' '}
              {trigger.action_orientation}
            </i>
            <AceEditor
              mode="sql"
              theme="github"
              name="trigger_action"
              value={trigger.action_statement}
              minLines={3}
              maxLines={100}
              width="100%"
              showPrintMargin={false}
              className={styles.add_mar_top_small}
            />
            {comment}
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
          removeFunc={isEventTriggerPGTrigger(trigger) ? null : onDelete}
          isCollapsable
        />
      </div>
    );
  });
};

export default TriggerEditorList;
