import { IconButton } from '@hasura/shared/ui';
import { FaRedoAlt } from 'react-icons/fa';

type RedeliverButtonProps = {
  onClickHandler: (e: React.MouseEvent) => void;
};

const RedeliverEventButton: React.FC<RedeliverButtonProps> = ({
  onClickHandler,
}) => (
  <IconButton mode="default" title="Redeliver event" onClick={onClickHandler}>
    <FaRedoAlt />
  </IconButton>
);

export default RedeliverEventButton;
