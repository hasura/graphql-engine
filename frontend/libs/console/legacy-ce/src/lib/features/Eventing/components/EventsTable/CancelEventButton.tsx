import { FaTimes } from 'react-icons/fa';
import { IconButton } from '@radix-ui/themes';

type CancelButtonProps = {
  id: string;
  onClickHandler: (e: React.MouseEvent) => void;
};

const CancelEventButton: React.FC<CancelButtonProps> = ({
  id,
  onClickHandler,
}) => (
  <IconButton
    id={id}
    onClick={onClickHandler}
    title="Cancel Event"
    variant="outline"
  >
    <FaTimes />
  </IconButton>
);

export default CancelEventButton;
