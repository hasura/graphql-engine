import clsx from 'clsx';
import React from 'react';

interface CardRadioGroupItem<T> {
  value: T;
  title: string;
  body: string | React.ReactNode;
}

interface CardRadioGroupProps<T> {
  value?: T;
  items: Array<CardRadioGroupItem<T>>;
  disabled?: boolean;
  orientation?: 'horizontal' | 'vertical';
  onChange: (option: T) => void;
}

export const CardRadioGroup = <T extends string = string>(
  props: CardRadioGroupProps<T>
) => {
  const { value, items, disabled = false, onChange } = props;

  return (
    <div
      className={clsx(
        'grid gap-sm',
        props.orientation === 'vertical'
          ? 'grid-cols-1'
          : 'grid-cols-[repeat(_auto-fit,_minmax(300px,_1fr))]'
      )}
    >
      {items.map(item => {
        const { value: iValue, title, body } = item;
        return (
          <div
            className={clsx(
              'bg-white shadow-sm rounded p-md border border-gray-300 flex',
              disabled ? 'cursor-not-allowed bg-gray-200' : 'cursor-pointer',
              value === iValue && 'ring-2 ring-yellow-200 border-yellow-400'
            )}
            key={iValue}
            onClick={() => !disabled && onChange(iValue)}
          >
            <div>
              <input
                id={`radio-select-${iValue}`}
                type="radio"
                value={iValue}
                x-model="relationType"
                className={clsx(
                  'rounded-full border shadow-sm border-gray-300 hover:border-gray-400 focus:ring-yellow-400',
                  disabled ? 'cursor-not-allowed bg-gray-200' : 'cursor-pointer'
                )}
                onChange={() => onChange(iValue)}
                checked={value === iValue}
                data-test={`radio-select-${iValue}`}
                disabled={disabled}
              />
            </div>
            <div className="ml-sm">
              <label
                htmlFor={`radio-select-${iValue}`}
                className={clsx(
                  'mb-sm font-semibold mt-0.5',
                  disabled ? 'cursor-not-allowed' : 'cursor-pointer'
                )}
              >
                {title}
              </label>
              <p className="text-muted">{body}</p>
            </div>
          </div>
        );
      })}
    </div>
  );
};
