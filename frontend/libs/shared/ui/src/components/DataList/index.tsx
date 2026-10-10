import { DataList as ThemeDataList } from '@radix-ui/themes';
import { ReactNode } from 'react';

export type DataListItemProps = ThemeDataList.ItemProps & {
  label: ReactNode;
  value: ReactNode;
};

export type DataListProps = ThemeDataList.RootProps & {
  items: DataListItemProps[];
  options?: {
    label?: ThemeDataList.LabelProps;
    value?: ThemeDataList.ValueProps;
  };
};

export const DataList = ({ items, options, ...rest }: DataListProps) => {
  return (
    <ThemeDataList.Root {...rest}>
      {items.map(({ label, value, ...itemProps }, i) => (
        <ThemeDataList.Item key={`data-list-item-${i}`} {...itemProps}>
          <ThemeDataList.Label {...options?.label}>{label}</ThemeDataList.Label>
          <ThemeDataList.Value {...options?.value}>{value}</ThemeDataList.Value>
        </ThemeDataList.Item>
      ))}
    </ThemeDataList.Root>
  );
};
