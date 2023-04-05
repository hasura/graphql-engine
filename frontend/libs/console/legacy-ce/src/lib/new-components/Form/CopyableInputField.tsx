import { ExtendInputFieldProps, InputField } from '.';
import clsx from 'clsx';
import React from 'react';
import { useFormContext } from 'react-hook-form';
import { FaRegCopy } from 'react-icons/fa';

const twStyles = {
  alignWithInput: `bottom-[22px]`,
  inputHeight: `h-[36px]`,
  buttonWidth: `w-[42px]`,
  copyConfirm: {
    base: `select-none transition-opacity duration-300 font-bold bg-slate-100/50 rounded backdrop-blur-sm absolute left-0 w-full flex items-center justify-center`,
    invisible: `opacity-0 pointer-events-none`,
    visible: `opacity-100 pointer-events-auto`,
  },
  copyButton: `text-gray-600 bg-gray-50 px-sm focus-visible:bg-blue-100 h-[calc(2.5rem-2px)] rounded-r outline-none active:opacity-50 border-none bg-transparent absolute right-[1px] shadow-none bg-none`,
  input: `pr-[42px]`,
};

type ExtendedType = ExtendInputFieldProps<{
  onCopy?: (currentValue: string) => void;
}>;

/**
 * Both `appendLabel` and `clearButton` are typed as `never` to prevent usage.
 * Due to an issue with `Omit` not working correctly with Unions, we are unable to simply `Omit` these properties from the type.
 * More information can be found here: https://github.com/microsoft/TypeScript/issues/31501#issuecomment-1079728677
 *
 * `appendLabel` and `clearButton` cannot be used as they all occupy the same UI space
 *
 * `iconPosition` may also not be used. The default position is `start` so not allowing this prop keeps any icons rendered at the start.
 * If positioned at the end, then it would occupy the same UI space as the copy button.
 *
 * `size` is also prohibited as it breaks the absolute position of the copy button
 */
type CopyableInputFieldProps = ExtendedType & {
  appendLabel?: never;
  clearButton?: never;
  iconPosition?: never;
  size?: never;
};

export const CopyableInputField = (props: CopyableInputFieldProps) => {
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

    props.onCopy?.(fieldValue);

    // show confirmation
    setShowCopiedConfirmation(true);

    // hide after 1.5s
    copyTimer.current = setTimeout(() => {
      setShowCopiedConfirmation(false);
    }, 1500);
  };

  return (
    <div className="relative">
      <InputField
        {...props}
        inputClassName={clsx(twStyles.input, props?.inputClassName)}
      />

      <button
        type="button"
        aria-label="Copy Text"
        data-testid="copy-button"
        disabled={!fieldValue}
        className={clsx(
          twStyles.copyButton,
          twStyles.alignWithInput,
          twStyles.buttonWidth
        )}
        onClick={handleCopyButton}
      >
        <FaRegCopy className="w-5 h-5" />
      </button>
      <div
        className={clsx(
          twStyles.copyConfirm.base,
          twStyles.alignWithInput,
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
