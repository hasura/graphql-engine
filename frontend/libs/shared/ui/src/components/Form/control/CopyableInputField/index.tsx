import { ExtendInputFieldProps, InputField } from '../InputField';
import { InputProps } from '../../base';
import React from 'react';
import { useFormContext } from 'react-hook-form';
import { FaRegCheckCircle, FaRegCopy } from 'react-icons/fa';
import { Flex } from '@radix-ui/themes';
import { IconButton } from '../../../Button';

type ExtendedType = ExtendInputFieldProps<{
  onCopy?: (currentValue: string) => void;
}>;

/**
 * Within `fieldProps`, `appendLabel`, `clearable`, `iconPosition`, and `size` are typed as `never` to prevent usage.
 * Due to an issue with `Omit` not working correctly with Unions, we are unable to simply `Omit` these properties from the type.
 * More information can be found here: https://github.com/microsoft/TypeScript/issues/31501#issuecomment-1079728677
 *
 * `appendLabel` and `clearable` cannot be used as they all occupy the same UI space
 *
 * `iconPosition` may also not be used. The default position is `start` so not allowing this prop keeps any icons rendered at the start.
 * If positioned at the end, then it would occupy the same UI space as the copy button.
 *
 * `size` is also prohibited as it breaks the absolute position of the copy button
 */
type CopyableInputFieldProps = Omit<ExtendedType, 'fieldProps'> & {
  fieldProps?: Omit<
    InputProps,
    'appendLabel' | 'clearable' | 'iconPosition' | 'size'
  > & {
    appendLabel?: never;
    clearable?: never;
    iconPosition?: never;
    size?: never;
  };
};

const iconClassName = 'w-4 h-4';

export const CopyableInputField = ({
  fieldProps,
  ...props
}: CopyableInputFieldProps) => {
  const { watch } = useFormContext();
  const fieldValue = watch(props.name);

  // state to control visibility of copy confirmation
  const [showCopiedConfirmation, setShowCopiedConfirmation] =
    React.useState(false);

  const copyTimer = React.useRef<NodeJS.Timeout>(undefined);

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
    <InputField
      {...props}
      fieldProps={{
        ...fieldProps,
        appendLabel: (
          <Flex align="center" className="pl-4 pr-2">
            <IconButton
              type="button"
              variant="ghost"
              color={showCopiedConfirmation ? 'green' : 'indigo'}
              aria-label="Copy Text"
              data-testid="copy-button"
              disabled={!fieldValue}
              onClick={handleCopyButton}
            >
              {showCopiedConfirmation ? (
                <FaRegCheckCircle className={iconClassName} />
              ) : (
                <FaRegCopy className={iconClassName} />
              )}
            </IconButton>
          </Flex>
        ),
      }}
    />
  );
};
