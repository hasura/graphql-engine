import React from 'react';
import { BreadcrumbItemView } from './BreadcrumbItemView';

export type BreadcrumbItem =
  | {
      title: React.ReactText;
      icon?: React.ReactElement;
      onClick?: () => void;
    }
  | string;

export type BreadcrumbsProps = {
  items: BreadcrumbItem[];
};

export const Breadcrumbs = ({ items }: BreadcrumbsProps) => (
  <div className="flex items-center gap-1" data-testid="breadcrumbs">
    {items.map((item, i) => (
      <BreadcrumbItemView
        key={typeof item === 'string' ? item : item.title}
        item={item}
        isLastItem={i === items.length - 1}
      />
    ))}
  </div>
);
