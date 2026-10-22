import React from 'react';
import { Text } from '@hasura/shared/ui';
import { Flex } from '@radix-ui/themes';

interface FunctionCommentEditorProps {
  comment: string | null | undefined;
}

/**
 * Displays a tracked function's comment, read-only.
 *
 * Function comments cannot be edited from the console. The metadata API
 * `<driver>_set_function_customization` only replaces the function
 * *configuration* (custom name, custom root fields, session argument, ...) and
 * has no `comment` field — unlike `<driver>_set_table_customization`, whose
 * `configuration` payload carries a `comment`. A function's comment can only be
 * set when the function is (re)tracked (the top-level `comment` of
 * `track_function`). Presenting an edit/save flow here would therefore let the
 * user "save" a comment that the backend silently drops, so we only ever show
 * an existing comment with no editing controls.
 *
 * When the function has no comment (empty, null or undefined) the component
 * renders nothing at all.
 */
export const FunctionCommentEditor: React.FC<FunctionCommentEditorProps> = ({
  comment,
}) => {
  if (!comment) {
    return null;
  }

  return (
    <div>
      <Flex className="mb-2" align="center" gap="2">
        <Text weight="bold">Function Comments</Text>
      </Flex>
      <Text as="div">{comment}</Text>
    </div>
  );
};

export default FunctionCommentEditor;
