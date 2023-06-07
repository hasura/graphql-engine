import React, { useState, useEffect } from 'react';
import * as DropdownMenu from '@radix-ui/react-dropdown-menu';
import clsx from 'clsx';
import { FaChevronDown } from 'react-icons/fa';

import { SELECT_ALL_NAME_DISPLAY } from '../../constants';
import styles from '../../Metrics.module.scss';
import { v4 as uuid } from 'uuid';

const DropdownComponent = (props: any) => {
  /* Accepts children which can be a single Menu Items
   * or array of Menu Items
   * TODO: How to validate whether the childrens are only of Menu item type?
   * */
  const [componentId] = useState(uuid());
  const [show, setShow] = useState(false);
  const {
    id,
    displayValue,
    options,
    onChange,
    selectedValues,
    children,
    selectAll,
  } = props;
  const selectedValuesList =
    Object.keys(selectedValues).length === options.length - 1
      ? Object.assign({ ...selectedValues }, { 'Select All': true })
      : selectedValues;

  // Force dropdown content to be same width as trigger
  useEffect(() => {
    const component = document.getElementById(componentId);
    document.documentElement.style.setProperty(
      `--radix-dropdown-menu-trigger-width-${componentId}`,
      component?.offsetWidth + 'px'
    );
  }, [displayValue, componentId]);

  const handleToggle = (open: boolean) => {
    setShow(open);
  };

  const handleChange = (val: string) => {
    if (
      val === SELECT_ALL_NAME_DISPLAY &&
      Object.keys(selectedValuesList).length === options.length
    ) {
      selectAll(id, options, true);
    } else if (val === SELECT_ALL_NAME_DISPLAY) {
      selectAll(id, options);
    } else {
      onChange(val);
    }
  };

  const renderTitle = (title: string) => {
    if (
      title === 'Select All' &&
      Object.keys(selectedValuesList).length === options.length
    ) {
      return 'Unselect All';
    }
    return title;
  };

  return (
    <DropdownMenu.Root open={show} onOpenChange={handleToggle}>
      <DropdownMenu.Trigger
        id={componentId}
        className={clsx(
          styles['dropDownBtn'],
          'flex justify-between gap-3 px-3 py-2 cursor-pointer',
          'font-normal block w-full h-input shadow-sm rounded border border-gray-300 hover:border-gray-400 focus-visible:outline-0 focus-visible:ring-2 focus-visible:ring-yellow-200 focus-visible:border-yellow-400 placeholder-gray-500'
        )}
      >
        <div
          className={
            Object.keys(selectedValuesList).length > 0 ? '' : 'text-gray-500'
          }
        >
          {displayValue}
        </div>
        <FaChevronDown className={clsx(show ? 'rotate-180' : 'rotate-0')} />
      </DropdownMenu.Trigger>

      <DropdownMenu.Content
        className="border border-slate-300 rounded bg-white cursor-pointer z-50"
        align="start"
        style={{
          minWidth: `var(--radix-dropdown-menu-trigger-width-${componentId})`,
        }}
      >
        {options.map((list: any, index: string) => {
          return (
            <DropdownMenu.Item
              key={displayValue + '_' + index}
              className="bg-white hover:bg-slate-200 px-3 py-2 border-slate-300 border-b font-sans"
              onSelect={() => handleChange(list.title as string)}
            >
              <div
                className={clsx(
                  styles['commonCheckBox'],
                  'flex flex-row items-baseline'
                )}
              >
                <input
                  id={id + '_' + index}
                  type="checkbox"
                  className="legacy-input-fix"
                  value={list.title}
                  onChange={() => {}}
                  checked={selectedValuesList[list?.title || ''] ? true : false}
                />
                <label
                  className={clsx(styles['eclipseText'], styles['fw_light'])}
                  htmlFor={id + '_' + index}
                >
                  {renderTitle(list.title as string)}
                </label>
              </div>
            </DropdownMenu.Item>
          );
        })}
        {children ? (
          <div className="bg-white hover:bg-slate-200 px-3 py-2 border-slate-300 border-b list-none font-sans">
            {children}
          </div>
        ) : null}
      </DropdownMenu.Content>
    </DropdownMenu.Root>
  );
};
export default DropdownComponent;
