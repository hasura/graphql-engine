import { useDocumentTitle } from '@hasura/shared/hooks';
import { useCurrentActionContext } from '../../context';
import ActionContainer from '../Containers/ActionContainer';
import ModifyActionForm from './Modify';
import { getModifyState } from './utils';
import { flattenCustomTypes } from '../../../../shared/utils/hasuraCustomTypeUtils';

const ModifyAction: React.FC = () => {
  const { currentAction, metadata } = useCurrentActionContext();
  useDocumentTitle(`Modify Action - ${currentAction.name} - Actions | Hasura`);

  const initialState = getModifyState(
    currentAction,
    metadata.custom_types ? flattenCustomTypes(metadata.custom_types) : [],
  );

  return (
    <ActionContainer tabName="modify">
      <ModifyActionForm initialState={initialState} />
    </ActionContainer>
  );
};

export default ModifyAction;
