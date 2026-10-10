import { Flex } from '@radix-ui/themes';
import { BreadcrumbItem, BreadcrumbItemView } from './BreadcrumbItemView';

export type BreadcrumbsProps = {
  className?: string;
  items: BreadcrumbItem[];
};

export const Breadcrumbs = ({ items, className }: BreadcrumbsProps) => (
  <Flex className={className} align="center" gap="1" data-testid="breadcrumbs">
    {items.map((item, i) => (
      <BreadcrumbItemView
        key={typeof item === 'string' ? item : item.title}
        item={item}
        isLastItem={i === items.length - 1}
      />
    ))}
  </Flex>
);
