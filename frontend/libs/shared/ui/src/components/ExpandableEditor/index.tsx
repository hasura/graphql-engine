import React, { useEffect, useState } from 'react';
import { Flex } from '@radix-ui/themes';
import { Button } from '../Button';
import { Text } from '../typography';

export type ExpandableEditorFunction = (
  success: () => void,
  error?: () => void,
) => void;

type Props = {
  toggled?: boolean | null;
  readOnlyMode?: boolean;
  isCollapsable?: boolean;
  collapseButtonText?: string;
  expandButtonText?: string;
  expandCallback?: () => void;
  collapseCallback?: () => void;
  dataTest?: string;
  service: string;
  property: string;
  ongoingRequest?: string;
  saveButtonText?: string;
  removeButtonText?: string;
  expandedLabel?: () => React.ReactNode;
  editorExpanded: () => React.ReactNode;
  collapsedLabel?: () => React.ReactNode;
  editorCollapsed?: () => React.ReactNode;
  saveFunc?: (success: () => void, error?: () => void) => void;
  removeFunc?: ((callback: () => void) => void) | null;
};

export const ExpandableEditor = ({
  service,
  property,
  ongoingRequest,
  dataTest,
  readOnlyMode,
  toggled,
  isCollapsable,
  expandButtonText = 'Edit',
  collapseButtonText = 'Close',
  saveButtonText = 'Save',
  removeButtonText = 'Remove',
  expandCallback,
  collapseCallback,
  saveFunc,
  removeFunc,
  expandedLabel,
  editorExpanded,
  collapsedLabel,
  editorCollapsed,
}: Props) => {
  const [isEditing, setIsEditing] = useState(toggled ?? false);

  useEffect(() => {
    if (toggled !== undefined && toggled !== null && toggled !== isEditing) {
      setIsEditing(toggled);
    }
  }, [toggled]);

  const toggleEditor = () => {
    if (expandCallback && !isEditing) {
      expandCallback();
    } else if (collapseCallback && isEditing) {
      collapseCallback();
    }

    setIsEditing(!isEditing);
  };

  const toggleButton = () => {
    if (isCollapsable === false && isEditing) {
      return null;
    }

    return (
      <Button
        mode="default"
        size="1"
        type="button"
        onClick={toggleEditor}
        disabled={readOnlyMode}
        data-test={dataTest}
      >
        {isEditing ? collapseButtonText : expandButtonText}
      </Button>
    );
  };

  const saveButton = () => {
    const isProcessing = ongoingRequest === property;
    const saveWithToggle = () => saveFunc?.(toggleEditor);
    return (
      <Button
        type="button"
        mode="default"
        loading={isProcessing}
        loadingText="Saving..."
        className="mr-2"
        onClick={saveWithToggle}
        data-test={`${service}-${property}-save`}
      >
        {saveButtonText}
      </Button>
    );
  };

  const removeButton = () => {
    const isProcessing = ongoingRequest === property;
    const removeWithToggle = () => removeFunc?.(toggleEditor);

    return (
      <Button
        type="button"
        mode="destructive"
        loading={isProcessing}
        loadingText="Removing..."
        onClick={removeWithToggle}
        data-test={`${service}-${property}-remove`}
      >
        {removeButtonText}
      </Button>
    );
  };

  const renderActionButtons = () => {
    return (
      <Flex align="center" gap="2" className="mt-4">
        {saveFunc && saveButton()}
        {removeFunc && removeButton()}
      </Flex>
    );
  };

  let editorLabel: React.ReactNode = null;
  let editorContent: React.ReactNode = null;
  let actionButtons: React.ReactNode = null;

  if (isEditing) {
    editorLabel = expandedLabel && expandedLabel();
    actionButtons = renderActionButtons();

    if (editorExpanded) {
      editorContent = <div>{editorExpanded()}</div>;
    }
  } else {
    editorLabel = collapsedLabel && collapsedLabel();

    if (editorCollapsed) {
      editorContent = <div>{editorCollapsed()}</div>;
    }
  }

  return (
    <>
      <Flex align="center" className="mb-2 pt-2" gap="4">
        {toggleButton()}
        <Text>{editorLabel}</Text>
      </Flex>
      {editorContent}
      {!readOnlyMode && actionButtons}
    </>
  );
};
