import { Dialog, DialogFooter, Text } from '@hasura/shared/ui';
import {
  CustomTypeObjectRelationship,
  CustomTypes,
} from '@hasura/shared/types';
import useRemoveActionRelationship from '../../../../Actions/hooks/useRemoveActionRelationship';
import { Strong } from '@radix-ui/themes';

type Props = {
  typeName: string;
  relationship: CustomTypeObjectRelationship;
  existingTypes: CustomTypes;
  onClose: () => void;
};

const ConfirmDeleteRelationshipDialog = ({
  typeName,
  relationship,
  existingTypes,
  onClose,
}: Props) => {
  const removeActionRel = useRemoveActionRelationship();

  return (
    <Dialog
      title="Confirm Action"
      description="Please confirm if you want to proceed with the action"
      onClose={onClose}
      size="sm"
      footer={
        <DialogFooter
          onSubmit={() => {
            removeActionRel(
              {
                existingTypes,
                relationshipName: relationship.name,
                typeName,
              },
              () => {
                onClose();
              },
            );
          }}
          onClose={onClose}
          callToDeny="Cancel"
          callToAction="Remove Relationship"
        />
      }
    >
      <div className="mb-2">
        <Text as="p">
          This will remove <Text color="red">{relationship.name}</Text> from
          type <Strong>{typeName}</Strong>. This will affect all the actions
          that use the type &quot;{typeName}&quot;. Please confirm you would
          like to go ahead with it.
        </Text>
      </div>
    </Dialog>
  );
};

export default ConfirmDeleteRelationshipDialog;
