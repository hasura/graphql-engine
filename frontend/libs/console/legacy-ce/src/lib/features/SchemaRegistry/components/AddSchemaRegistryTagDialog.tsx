import React from 'react';
import { Flex } from '@radix-ui/themes';
import { Dialog, Input, DialogFooter } from '@hasura/shared/ui';
import { HexColorPicker } from 'react-colorful';
import { DEFAULT_TAG_COLOR } from '../constants';
import globals from '../../../Globals';
import { Analytics, REDACT_EVERYTHING } from '@hasura/shared/analytics';
import { useCreateSchemaTag } from '../hooks/useCreateSchemaTag';
import { SchemaRegistryTag } from '../types';

interface AddSchemaRegistryTagDialogProps {
  tagsList: SchemaRegistryTag[];
  setTagsList: React.Dispatch<React.SetStateAction<SchemaRegistryTag[]>>;
  onClose: () => void;
  entryHash: string;
}

export const AddSchemaRegistryTagDialog: React.FC<
  AddSchemaRegistryTagDialogProps
> = (props) => {
  const { tagsList, setTagsList, onClose, entryHash } = props;
  const projectID = globals.hasuraCloudProjectId;

  const [tag, setExistingTag] = React.useState<string>('');
  const [selectedColor, setSelectedColor] = React.useState(DEFAULT_TAG_COLOR);

  const onSuccess = (createdTag: SchemaRegistryTag) => {
    const newTag: SchemaRegistryTag = {
      id: createdTag.id,
      name: createdTag.name,
      color: createdTag.color,
    };

    const newTagList = [...tagsList, newTag];
    setTagsList(newTagList);
    onClose();
  };

  const { createSchemaRegistryTagMutation } = useCreateSchemaTag(onSuccess);

  const onCreateTagSubmit = React.useCallback(() => {
    createSchemaRegistryTagMutation.mutate({
      tagName: tag,
      projectId: projectID || '',
      entryHash: entryHash,
      color: selectedColor,
    });
  }, [tag, selectedColor]);

  const handleColorChange = (color: string) => {
    setSelectedColor(color);
  };

  const handleOnChangeTag = (e: React.ChangeEvent<HTMLInputElement>) =>
    setExistingTag(e.target.value);

  return (
    <Dialog size="sm" title="Create a Tag" onClose={onClose}>
      <>
        <Analytics name="AddSchemaRegistryTagDialog" {...REDACT_EVERYTHING}>
          <Flex direction="column" justify="center" className="p-4">
            <div className="w-full">
              <Input
                name="schema-registry-tag"
                placeholder="Type to create a tag"
                value={tag}
                onChange={handleOnChangeTag}
                data-test="schema-registry-tag-input"
              />
            </div>
            {tag && (
              <Flex justify="center" align="center" className="mt-4">
                <Flex className="mb-4">
                  <Input
                    name="schema-registry-tag-color"
                    value={selectedColor}
                    className="w-full font-bold"
                    placeholder="Tag Color"
                    type="text"
                    onChange={(e: React.BaseSyntheticEvent) =>
                      handleColorChange(e.target.value)
                    }
                    data-test="schema-registry-tag-color-input"
                  />
                </Flex>
                <Flex className="mt-[-8px] ml-8">
                  <HexColorPicker
                    color={selectedColor}
                    onChange={handleColorChange}
                  />
                </Flex>
              </Flex>
            )}
          </Flex>
        </Analytics>
        <DialogFooter
          callToDeny="Cancel"
          callToAction="Create and Assign"
          onClose={onClose}
          onSubmit={() => {
            onCreateTagSubmit();
          }}
        />
      </>
    </Dialog>
  );
};
