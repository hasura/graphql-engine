import { Flex } from '@radix-ui/themes';
import { FaLink, FaPen } from 'react-icons/fa';

type InputModeToggleProps = {
  showNeonButton: boolean;
  toggleShowNeonButton: VoidFunction;
  disabled?: boolean;
};

export function InputModeToggle(props: InputModeToggleProps) {
  const { showNeonButton, toggleShowNeonButton, disabled } = props;
  const handleClick = () => {
    if (!disabled) toggleShowNeonButton();
  };

  return (
    <Flex
      align="center"
      gap="2"
      onClick={handleClick}
      className={`font-[350] ${
        disabled
          ? 'cursor-not-allowed text-gray-600'
          : 'cursor-pointer text-cloud-dark hover:text-cloud-darker'
      }`}
    >
      {showNeonButton ? (
        <>
          <FaLink /> Connect Existing Database
        </>
      ) : (
        <>
          <FaPen /> Create New Database
        </>
      )}
    </Flex>
  );
}
