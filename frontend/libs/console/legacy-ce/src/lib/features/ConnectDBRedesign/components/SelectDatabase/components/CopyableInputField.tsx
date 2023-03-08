import { Button } from '../../../../../new-components/Button';
import { InputField } from '../../../../../new-components/Form';
import clsx from 'clsx';
import React from 'react';
import { useFormContext } from 'react-hook-form';
import { FaRegCopy } from 'react-icons/fa';

const twStyles = {
  alignToTopOfInput: `top-[32px]`,
  inputHeight: `h-[36px]`,
  copyConfirm: {
    base: `select-none transition-opacity duration-300 font-bold bg-slate-100/50 rounded backdrop-blur-sm absolute left-0 w-full flex items-center justify-center`,
    invisible: `opacity-0 pointer-events-none`,
    visible: `opacity-100 pointer-events-auto`,
  },
  copyButton: `active:opacity-50 border-none bg-transparent absolute right-0 shadow-none bg-none`,
};

export const CopyableInputField: typeof InputField = props => {
  const { watch } = useFormContext();
  const fieldValue = watch(props.name);

  // state to control visibility of copy confirmation
  const [showCopiedConfirmation, setShowCopiedConfirmation] =
    React.useState(false);

  const copyTimer = React.useRef<NodeJS.Timeout>();

  const handleCopyButton = () => {
    // clear timer if already going...
    if (copyTimer.current) {
      clearTimeout(copyTimer.current);
    }

    // copy text to clipboard
    navigator.clipboard.writeText(fieldValue);

    // show confirmation
    setShowCopiedConfirmation(true);

    // hide after 1.5s
    copyTimer.current = setTimeout(() => {
      setShowCopiedConfirmation(false);
    }, 1500);
  };
  return (
    <div className="relative">
      <InputField {...props} />
      <Button
        className={clsx(twStyles.copyButton, twStyles.alignToTopOfInput)}
        icon={<FaRegCopy />}
        onClick={handleCopyButton}
      />
      <div
        className={clsx(
          twStyles.copyConfirm.base,
          twStyles.alignToTopOfInput,
          twStyles.inputHeight,
          twStyles.copyConfirm.invisible,
          showCopiedConfirmation && twStyles.copyConfirm.visible
        )}
      >
        Copied!
      </div>
    </div>
  );
};
