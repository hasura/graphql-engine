import React from 'react';
import AceEditor from 'react-ace';

import ExpandableEditor from '../../../Common/Layout/ExpandableEditor/Editor';
import { deleteTrigger } from './ModifyActions';
import { getConfirmation } from '../../../Common/utils/jsUtils';

const TriggerEditorList = ({ tableSchema, dispatch }) => {
  const triggers = tableSchema.triggers;

  if (!triggers.length) {
    return <div className="italic text-sm text-gray-600">(No triggers)</div>;
  }

  // HACK
  const isEventTriggerPGTrigger = trigger =>
    trigger.action_statement.includes('hdb_views');

  // NOTE: regarding object properties of the values within the `triggers` list
  // action_timing      -> 'AFTER' | 'BEFORE'
  // event_manipulation -> 'INSERT' | 'UPDATE' | 'DELETE'
  // NOTE: Sorting & Ordering of the triggers based on occurrance and Postgres specific order of execution
  // 1. Grouping all triggers based on the `action_timing` and `event_manipulation`. (BEFORE INSERT, BEFORE UPDATE, ...)
  // 2. Sorting of the names of the triggers within each of the groups
  // 3. Flatten the resultant list of lists
  const emptyClassificationObject = {
    BEFORE: {
      INSERT: [],
      UPDATE: [],
      DELETE: [],
    },
    AFTER: {
      INSERT: [],
      UPDATE: [],
      DELETE: [],
    },
  };
  const groupedTriggers = triggers.reduce(
    (acc, trigger) => ({
      ...acc,
      [trigger.action_timing]: {
        ...acc[trigger.action_timing],
        [trigger.event_manipulation]: [
          ...acc[trigger.action_timing][trigger.event_manipulation],
          trigger,
        ],
      },
    }),
    emptyClassificationObject
  );
  const groupedList = [
    groupedTriggers.BEFORE.INSERT,
    groupedTriggers.BEFORE.UPDATE,
    groupedTriggers.BEFORE.DELETE,
    groupedTriggers.AFTER.INSERT,
    groupedTriggers.AFTER.UPDATE,
    groupedTriggers.AFTER.DELETE,
  ];
  const sortedAndOrderedTriggers = groupedList
    .map(triggersList =>
      triggersList.sort((a, b) => (a.trigger_name > b.trigger_name ? 1 : -1))
    )
    .reduce((acc, triggerList) => [...acc, ...triggerList], []);
  return sortedAndOrderedTriggers.map((trigger, i) => {
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
        <div className="flex items-center">
          <span className="font-semibold mr-sm">{triggerName}</span>
          <span className="mr-sm">-</span>
          <span>
            {trigger.action_timing} {trigger.event_manipulation}
          </span>
        </div>
        <div className="text-gray-400 text-sm">{trigger.comment}</div>
      </div>
    );

    const expandedLabel = () => <b>{triggerName}</b>;

    const expandedContent = () => {
      let commentText = null;
      if (trigger.comment) {
        commentText = trigger.comment;
      } else if (isEventTriggerPGTrigger(trigger)) {
        commentText = 'This is a custom trigger generated for an Event trigger';
      }
      return (
        <div>
          <div>
            <i>
              {trigger.action_timing} {trigger.event_manipulation}, FOR EACH{' '}
              {trigger.action_orientation}{' '}
              {trigger.action_condition
                ? ' WHEN ' + trigger.action_condition
                : ''}
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
              className="mt-xs"
              setOptions={{ useWorker: false }}
            />
            {commentText && (
              <div className="text-gray-500 mt-sm">{commentText}</div>
            )}
          </div>
        </div>
      );
    };

    return (
      <div key={`trigger-${i}`}>
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
