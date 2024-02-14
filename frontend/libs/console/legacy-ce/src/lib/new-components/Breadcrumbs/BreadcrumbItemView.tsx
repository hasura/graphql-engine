import clsx from 'clsx';
import React from 'react';
import { FaAngleRight } from 'react-icons/fa';
import { BreadcrumbItem } from './Breadcrumbs';

const twItemStyle = {
  default: `flex items-center text-muted select-none p-1 px-2 rounded whitespace-nowrap`,
  link: `cursor-pointer text-secondary hover:bg-secondary/10 hover:text-secondary-dark active:bg-secondary/5 focus-visible:outline focus-visible:outline-2 focus-visible:outline-secondary/50`,
  lastItem: `font-semibold text-yellow-500`,
};

export function BreadcrumbItemView({
  item,
  isLastItem,
}: {
  item: BreadcrumbItem;
  isLastItem: boolean;
}) {
  const content = () => (
    <>
      {typeof item !== 'string' && !!item.icon && item.icon}
      <span
        className={clsx(
          typeof item !== 'string' && !!item.icon && 'ml-1.5',
          isLastItem && twItemStyle.lastItem
        )}
      >
        {typeof item === 'string' ? item : item.title}
      </span>
    </>
  );

  const isLink = typeof item === 'string' || !item.onClick;

  return (
    <>
      {isLink ? (
        <div className={clsx(twItemStyle.default)}>{content()}</div>
      ) : (
        <button
          onClick={item.onClick}
          className={clsx(twItemStyle.default, twItemStyle.link)}
        >
          {content()}
        </button>
      )}
      {!isLastItem && <FaAngleRight className="text-muted" />}
    </>
  );
}
