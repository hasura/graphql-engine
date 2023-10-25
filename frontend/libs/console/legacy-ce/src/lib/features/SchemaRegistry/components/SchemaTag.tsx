import React from 'react';
import { FaTimes } from 'react-icons/fa';
import { useDeleteSchemaTag } from '../hooks/useDeleteSchemaTag';
import { SchemaRegistryTag } from '../types';
import { hexToRGB } from '../utils';

interface TagProps {
  schemaRegistryTag: SchemaRegistryTag;
  onRemove: (id: string) => void;
}

export const SchemaTag: React.FC<TagProps> = props => {
  const { schemaRegistryTag, onRemove } = props;
  const { id, color, name } = schemaRegistryTag;

  const { deleteSchemaRegistryTagMutation } = useDeleteSchemaTag(onRemove);

  const onDelete = React.useCallback(() => {
    deleteSchemaRegistryTagMutation.mutate({
      ID: id,
    });
  }, [schemaRegistryTag]);

  const rgbaColorValue = hexToRGB(color, 0.2);
  return (
    <div
      data-testid="tag"
      className="inline-flex items-center px-xs py-0.5 rounded text-sm tracking-wide font-semibold"
      style={{ backgroundColor: rgbaColorValue }}
    >
      <div style={{ color: color }} className="mr-xs">
        {name}
      </div>
      <FaTimes
        className="fill-current cursor-pointer text-muted hover:text-gray-800 mt-[1px]"
        onClick={onDelete}
      />
    </div>
  );
};
