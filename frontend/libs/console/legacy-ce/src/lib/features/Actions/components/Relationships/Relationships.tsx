import { useState } from 'react';
import { FaPlusCircle } from 'react-icons/fa';
import { useDocumentTitle } from '@hasura/shared/hooks';
import { useAppContext } from '@hasura/shared/context';
import { FlattenCustomType } from '../../../../shared/utils/hasuraCustomTypeUtils';
import {
  Action,
  CustomTypeObjectRelationship,
  Metadata,
} from '@hasura/shared/types';
import { Button, Text } from '@hasura/shared/ui';
import RelationshipsTable from './Components/RelationshipsTable';
import RelationshipFormDialog from './Components/RelationshipFormDialog';
import ConfirmDeleteRelationshipDialog from './Components/ConfirmDeleteRelationshipDialog';

type Props = {
  outputType: FlattenCustomType;
  currentAction: Action;
  metadata: Metadata['metadata'];
};

type DialogState =
  | { mode: 'add' }
  | { mode: 'edit'; relationship: CustomTypeObjectRelationship }
  | { mode: 'remove'; relationship: CustomTypeObjectRelationship }
  | undefined;

const Relationships = ({ outputType, currentAction, metadata }: Props) => {
  useDocumentTitle(`Relationships - ${currentAction.name} - Actions | Hasura`);

  const { readOnlyMode } = useAppContext();
  const [dialogState, setDialogState] = useState<DialogState>(undefined);

  const onClose = () => setDialogState(undefined);

  const relationships =
    outputType.kind === 'objects'
      ? (outputType.definition.relationships ?? [])
      : [];

  return (
    <div>
      {outputType.kind !== 'objects' ? (
        <Text size="1">
          Action relationships with scalar output types are not possible{' '}
        </Text>
      ) : (
        <>
          <RelationshipsTable
            typeName={outputType.definition.name}
            relationships={relationships}
            readOnlyMode={readOnlyMode}
            onEdit={(relationship) =>
              setDialogState({ mode: 'edit', relationship })
            }
            onRemove={(relationship) =>
              setDialogState({ mode: 'remove', relationship })
            }
          />

          {!readOnlyMode && (
            <div className="mt-4">
              <Button
                mode="default"
                type="button"
                leftIcon={FaPlusCircle}
                onClick={() => setDialogState({ mode: 'add' })}
              >
                Add Relationship
              </Button>
            </div>
          )}

          {dialogState?.mode === 'add' && (
            <RelationshipFormDialog
              outputType={outputType}
              metadata={metadata}
              onClose={onClose}
            />
          )}

          {dialogState?.mode === 'edit' && (
            <RelationshipFormDialog
              outputType={outputType}
              existingRelConfig={dialogState.relationship}
              metadata={metadata}
              onClose={onClose}
            />
          )}

          {dialogState?.mode === 'remove' && (
            <ConfirmDeleteRelationshipDialog
              typeName={outputType.definition.name}
              relationship={dialogState.relationship}
              existingTypes={metadata.custom_types ?? {}}
              onClose={onClose}
            />
          )}
        </>
      )}
    </div>
  );
};

export default Relationships;
