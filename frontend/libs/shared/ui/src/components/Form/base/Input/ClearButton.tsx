import { BsXCircleFill } from 'react-icons/bs';
import { IconButton, IconButtonProps } from '../../../Button';

type ClearButtonProps = Omit<IconButtonProps, 'icon'>;

const ClearButton: React.FC<ClearButtonProps> = (props) => {
  return (
    <IconButton {...props} variant="ghost" color="gray" radius="full">
      <BsXCircleFill />
    </IconButton>
  );
};

export default ClearButton;
