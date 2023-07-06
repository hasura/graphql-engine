import clsx from 'clsx';
import React from 'react';
import { IconType } from 'react-icons';
import {
  default as ReactSelectOriginal,
  GroupBase,
  Props as ReactSelectPropsOriginal,
  MenuListProps,
} from 'react-select';
import { VariableSizeList, areEqual } from 'react-window';

export type ReactSelectOptionType = {
  icon?: IconType;
  label: string;
  value: any;
  isDisabled?: boolean;
};

export type ReactSelectProps<
  Option,
  IsMulti extends boolean = false,
  Group extends GroupBase<Option> = GroupBase<Option>
> = ReactSelectPropsOriginal<Option, IsMulti, Group> & {
  isInvalid?: boolean;
};

export const ReactSelect = <IsMulti extends boolean = false>({
  isSearchable = true,
  isClearable = true,
  isInvalid,
  isDisabled,
  ...props
}: ReactSelectProps<
  ReactSelectOptionType,
  IsMulti,
  GroupBase<ReactSelectOptionType>
>) => {
  const formatOptionLabel = ({ label, icon }: ReactSelectOptionType) => (
    <div>
      {icon && (
        <span className="mr-2 -translate-y-0.5 inline-block">
          {React.createElement(icon)}
        </span>
      )}
      <span>{label}</span>
    </div>
  );

  const MenuList = (
    props: MenuListProps<
      ReactSelectOptionType,
      IsMulti,
      GroupBase<ReactSelectOptionType>
    >
  ) => {
    const { children, maxHeight = '300px' } = props;

    const [childrenArray, setChildrenArray] = React.useState<
      React.ReactElement[]
    >([]);

    React.useEffect(() => {
      const childrenArray = React.Children.toArray(
        React.Children.toArray(children)
      ) as React.ReactElement[];
      const flatChildrenArray: React.ReactElement[] = [];

      childrenArray.forEach((child: React.ReactElement) => {
        if (child?.key?.toString().includes('group')) {
          flatChildrenArray.push({
            ...child,
            props: { ...child.props, children: undefined },
          });
          if (child?.props?.children) {
            flatChildrenArray.push(...child.props.children);
          }
        } else {
          flatChildrenArray.push(child);
        }
      });
      setChildrenArray(flatChildrenArray);
    }, [children]);

    const Row = React.memo(
      ({ index, style }: { index: number; style: React.CSSProperties }) => {
        return <div style={style}>{childrenArray[index]}</div>;
      },
      areEqual
    );

    return (
      <VariableSizeList
        height={maxHeight}
        itemCount={childrenArray.length}
        itemSize={index =>
          childrenArray[index].key?.toString().includes('group') ? 24 : 37
        }
        width="100%"
        initialScrollOffset={0}
        style={{ maxHeight: '300px', height: 'min-content' }}
      >
        {Row}
      </VariableSizeList>
    );
  };

  return (
    <ReactSelectOriginal
      {...props}
      components={{ MenuList }}
      unstyled
      isSearchable={isSearchable}
      isClearable={isClearable}
      formatOptionLabel={formatOptionLabel}
      classNames={{
        container: () =>
          clsx(
            'block',
            'w-full',
            'h-input',
            'shadow-sm',
            'rounded',
            'bg-white',
            'border',
            'hover:border-slate-400',
            'focus-within:outline-0',
            'focus-within:ring-2',
            'focus-within:ring-yellow-200',
            'focus-within:border-yellow-400',
            isInvalid
              ? clsx(
                  'border-red-600',
                  'hover:border-red-600',
                  'focus-within:border-red-600',
                  'focus-within:hover:border-red-600'
                )
              : 'focus-within:border-yellow-400 border-slate-300 focus-within:hover:border-yellow-400',
            isDisabled
              ? 'pointer-events-none bg-slate-200 border-slate-200 hover:border-slate-200'
              : ''
          ),
        control: () => clsx('!min-h-0'),
        valueContainer: state =>
          clsx(
            'px-[10.5px]',
            'py-[6px]',
            state.hasValue ? 'text-black' : 'text-slate-500',
            isDisabled ? 'pointer-events-none' : ''
          ),
        multiValue: () =>
          clsx(
            'bg-slate-200',
            'text-slate-800',
            'rounded',
            'gap-2',
            'px-2',
            'mr-1'
          ),
        menu: () =>
          clsx(
            'mt-2',
            'rounded',
            'shadow-md',
            'bg-white',
            'ring-1',
            'ring-slate-300',
            'divide-y',
            'divide-slate-300',
            'focus:outline-none',
            'overflow-hidden'
          ),
        groupHeading: () =>
          clsx('text-slate-500 text-sm bg-slate-100', 'py-1', 'px-2'),
        option: state =>
          clsx(
            'relative',
            'cursor-pointer',
            'flex',
            'items-center',
            'px-xs',
            'py-xs',
            'rounded',
            'whitespace-nowrap',
            state.isFocused ? 'bg-amber-50' : '',
            state.isDisabled ? 'opacity-40 italic' : '',
            state.isSelected
              ? clsx(
                  'pl-5',
                  'before:absolute',
                  'before:inset-0',
                  'before:top-1/2',
                  'before:-translate-y-1/2',
                  'before:h-4',
                  'before:border-transparent',
                  'before:border-l-slate-300',
                  'before:border-8'
                )
              : ''
          ),
        noOptionsMessage: () => clsx('text-slate-500 p-4'),
        indicatorsContainer: () =>
          clsx('text-slate-500', 'flex', 'justify-center', 'px-3', 'gap-3'),
        indicatorSeparator: () => clsx('bg-slate-300', 'my-1', 'w-px'),
      }}
    />
  );
};
