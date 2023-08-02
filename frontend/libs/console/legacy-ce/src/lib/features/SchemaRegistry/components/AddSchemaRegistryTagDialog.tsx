import React from 'react';
import { Dialog } from '../../../new-components/Dialog';
import { HuePicker, ColorResult } from 'react-color';
import { DEFAULT_TAG_COLOR } from '../constants';
import globals from '../../../Globals';
import { Analytics, REDACT_EVERYTHING } from '../../Analytics';
import { useCreateSchemaTag } from '../hooks/useCreateSchemaTag';
import { SchemaRegistryTag } from '../types';
import { Input } from '../../../new-components/Form';

interface AddSchemaRegistryTagDialogProps {
  tagsList: SchemaRegistryTag[];
  setTagsList: React.Dispatch<React.SetStateAction<SchemaRegistryTag[]>>;
  onClose: () => void;
  entryHash: string;
}

export const AddSchemaRegistryTagDialog: React.FC<
  AddSchemaRegistryTagDialogProps
> = props => {
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
    <Dialog size="sm" hasBackdrop title="Create a Tag" onClose={onClose}>
      <>
        <Analytics name="AddSchemaRegistryTagDialog" {...REDACT_EVERYTHING}>
          <div className="flex flex-col justify-center p-4">
            <div className="w-full">
              <Input
                name="schema-registry-tag"
                placeholder="Type to create a tag"
                fieldProps={{ value: tag }}
                onChange={handleOnChangeTag}
                data-test="schema-registry-tag-input"
              />
            </div>
            {tag && (
              <div className="flex flex-row justify-center items-center mt-4">
                <div className="flex mb-4">
                  <Input
                    name="schema-registry-tag-color"
                    fieldProps={{ value: selectedColor }}
                    className="w-full font-bold"
                    placeholder="Tag Color"
                    type="text"
                    onChange={(e: React.BaseSyntheticEvent) =>
                      handleColorChange(e.target.value)
                    }
                    data-test="schema-registry-tag-color-input"
                  />
                </div>
                <div className="flex mt-[-8px] ml-8">
                  <HuePicker
                    color={selectedColor}
                    onChangeComplete={(r: ColorResult) =>
                      handleColorChange(r.hex)
                    }
                  />
                </div>
              </div>
            )}
          </div>
        </Analytics>
        <Dialog.Footer
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
