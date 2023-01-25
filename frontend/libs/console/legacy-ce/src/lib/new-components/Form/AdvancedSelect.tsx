import React, { ComponentProps, FocusEventHandler, ReactElement } from 'react';
import clsx from 'clsx';
import {
  ControllerRenderProps,
  FieldValues,
  RefCallBack,
} from 'react-hook-form';
import Select, {
  Props as SelectProps,
  OptionsType,
  OptionTypeBase,
  components,
} from 'react-select';
import { FiX, FiChevronDown } from 'react-icons/fi';
import { Tooltip } from '@/new-components/Tooltip';
import { FieldWrapperPassThroughProps } from './FieldWrapper';

export type MultiSelectItem = {
  label: string | ReactElement;
  value: any;
  disabled?: boolean;
};

export type AdvancedSelectProps = FieldWrapperPassThroughProps & {
  /**
   * The field name
   */
  name: string;
  /**
   * The options to display in the select
   */
  options: MultiSelectItem[];
  /**
   * The placeholder text to display when the select is not valued
   */
  placeholder?: string;
  /**
   * Flag to indicate if the select is disabled
   */
  disabled?: boolean;
  /**
   * The default value
   */
  defaultValue?: MultiSelectItem | null;
  /**
   * The value
   */
  value?: string;
  /**
   * Flag to indicate if the dropdown is multi values
   */
  isMulti?: boolean;
  /**
   * On blur callback
   */
  onChange?: SelectProps<OptionTypeBase>['onChange'];
  /**
   * On blur callback
   */
  onBlur?: FocusEventHandler;
  /**
   * Prop to be used with react-hook-form
   */
  field?: ControllerRenderProps<FieldValues, string>;
  /**
   * Useful to be used with react-hook-form
   */
  ref?: RefCallBack;
};

const customComponents: ComponentProps<typeof Select>['components'] = {
  DropdownIndicator: props => {
    const { className } = props;
    return (
      <components.DropdownIndicator
        {...props}
        className={clsx(className, 'text-gray-500 hover:text-gray-500')}
      >
        <FiChevronDown />
      </components.DropdownIndicator>
    );
  },
  Option: props => {
    return (
      <components.Option
        {...props}
        className={clsx(
          props.className,
          'flex items-center px-xs py-xs rounded whitespace-nowrap',
          props.isDisabled
            ? 'cursor-not-allowed text-gray-200'
            : 'cursor-pointer hover:bg-gray-100',
          props.isFocused && props.isDisabled ? 'bg-transparent' : '',
          props.isFocused && !props.isDisabled ? 'bg-slate-200' : '',
          props.isSelected && 'bg-slate-200 text-slate-800'
        )}
      />
    );
  },
  ValueContainer: props => {
    const { className } = props;
    return (
      <components.ValueContainer
        {...props}
        className={clsx(className, 'p-0')}
      />
    );
  },
  Control: props => {
    const { className } = props;
    return (
      <components.Control
        {...props}
        className={clsx(
          className,
          'flex h-full items-center justify-between px-2 rounded border border-gray-300 hover:border-gray-400',
          props.isFocused && 'ring-2 ring-yellow-200 border-yellow-400'
        )}
      />
    );
  },
  ClearIndicator: props => {
    const { className } = props;
    return (
      <components.ClearIndicator
        {...props}
        className={clsx(className, 'text-gray-500')}
      >
        <FiX />
      </components.ClearIndicator>
    );
  },
  MenuList: props => {
    const { className } = props;
    return (
      <components.MenuList {...props} className={clsx(className, 'px-1')} />
    );
  },
  MultiValueContainer: props => {
    const { className } = props;
    return (
      <components.MultiValueContainer
        {...props}
        className={clsx(className, 'bg-gray-200 m-0')}
      />
    );
  },
  IndicatorSeparator: () => null,
  MultiValueLabel: props => {
    const { className, children, ...rest } = props;
    // Display a tooltip if the label is too long
    const Wrapper = children.length > 20 ? Tooltip : React.Fragment;
    return (
      <Wrapper tooltipContentChildren={children}>
        <components.MultiValueLabel
          {...rest}
          className={clsx(className, 'text-black')}
        >
          {children}
        </components.MultiValueLabel>
      </Wrapper>
    );
  },
};

export const AdvancedSelect: React.VFC<AdvancedSelectProps> = ({
  name,
  ref,
  options,
  placeholder,
  dataTest,
  disabled = false,
  value,
  defaultValue,
  isMulti = false,
  field,
  onBlur,
  onChange,
}) => {
  // Convert the options to the format that react-select expects
  const selectOptions: OptionsType<OptionTypeBase> = options.map(option => {
    return {
      value: option.value,
      label: option.label,
      isDisabled: option.disabled,
    };
  });

  return (
    <Select
      {...field}
      ref={ref}
      name={name}
      onBlur={onBlur}
      onChange={(_value, action) => {
        if (field?.onChange) {
          field?.onChange(_value, action);
        }
        if (onChange) {
          onChange(_value, action);
        }
      }}
      defaultValue={defaultValue}
      id={name}
      isMulti={isMulti}
      className={clsx(
        'block w-full h-input shadow-sm rounded border-gray-300 hover:border-gray-400 focus-visible:outline-0 focus-visible:ring-2 focus-visible:ring-yellow-200 focus-visible:border-yellow-400',
        value?.length === 0 ? 'text-black' : 'text-gray-500',
        disabled
          ? 'cursor-not-allowed bg-slate-200 border-slate-200 hover:border-gray-200'
          : 'hover:border-slate-400'
      )}
      components={customComponents}
      classNamePrefix="select"
      data-test={dataTest}
      data-testid={name}
      options={selectOptions}
      isDisabled={disabled}
      styles={{
        multiValueLabel: baseStyles => ({
          ...baseStyles,
          maxWidth: 100,
        }),
      }}
      placeholder={placeholder}
      theme={theme => ({
        ...theme,
        spacing: {
          ...theme.spacing,
          controlHeight: 26, // h-input - py-xs
        },
      })}
    />
  );
};
