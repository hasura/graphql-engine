import * as RadixSwitch from '@radix-ui/react-switch';
import clsx from 'clsx';

export const Switch = ({ className, ...props }: RadixSwitch.SwitchProps) => {
  const { checked, disabled } = props;
  return (
    <RadixSwitch.Root
      className={clsx(
        checked ? 'bg-green-600' : 'bg-gray-200',
        disabled ? 'cursor-not-allowed opacity-40' : 'cursor-pointer',
        'relative inline-flex shrink-0 h-6 w-11 border-2 border-transparent rounded-full cursor-pointer transition-colors ease-in-out duration-200 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-amber-500',
        className
      )}
      {...props}
    >
      <RadixSwitch.Thumb
        className={clsx(
          checked ? 'translate-x-5' : 'translate-x-0',
          'pointer-events-none inline-block h-5 w-5 rounded-full bg-white shadow transform ring-0 transition ease-in-out duration-200'
        )}
      />
    </RadixSwitch.Root>
  );
};
