import { Analytics, REDACT_EVERYTHING } from '@hasura/shared/analytics';
import ActionContainer from '../../../../features/Actions/components/Containers/ActionContainer';
import { getScalarOutputType } from './utils';
import { unwrapType } from '../../../../shared/utils/wrappingTypeUtils';
import AllRelationships from './Relationships';
import { flattenCustomTypes } from '../../../../shared/utils/hasuraCustomTypeUtils';
import { useCurrentActionContext } from '../../context';

const ActionRelationships = () => {
  const { currentAction, metadata } = useCurrentActionContext();
  const allTypes = flattenCustomTypes(metadata.custom_types);

  const actionOutputTypeName = unwrapType(
    currentAction.definition.output_type,
  ).typename;

  const actionOutputType =
    allTypes.find((t) => t.definition.name === actionOutputTypeName) ??
    getScalarOutputType(actionOutputTypeName);

  return (
    <ActionContainer tabName="relationships">
      <Analytics name="ActionsRelationships" {...REDACT_EVERYTHING}>
        <div>
          <AllRelationships
            outputType={actionOutputType}
            currentAction={currentAction}
            metadata={metadata}
          />
        </div>
      </Analytics>
    </ActionContainer>
  );
};

export default ActionRelationships;
