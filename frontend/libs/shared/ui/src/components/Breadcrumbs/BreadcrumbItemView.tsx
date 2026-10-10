import { Flex } from '@radix-ui/themes';
import { FaAngleRight } from 'react-icons/fa';
import { To } from 'react-router';
import { Text } from '../typography';
import { RelativeLink } from '../link';

export type BreadcrumbItem =
  | {
      title: string;
      icon?: React.ReactElement<any>;
      url?: To;
      onClick?: () => void;
    }
  | string;

export function BreadcrumbItemView({
  item,
  isLastItem,
}: {
  item: BreadcrumbItem;
  isLastItem: boolean;
}) {
  const icon = typeof item !== 'string' ? item.icon : undefined;
  const title = typeof item === 'string' ? item : item.title;
  const onClick = typeof item !== 'string' ? item.onClick : undefined;
  const link = typeof item !== 'string' ? item.url : undefined;

  const textContent = (
    <Text
      weight={isLastItem ? 'bold' : 'regular'}
      color={isLastItem ? 'indigo' : 'gray'}
    >
      {title}
    </Text>
  );

  const content = (
    <Flex align="center" gap="1" onClick={onClick}>
      {icon}
      {link ? (
        <Text asChild color="gray" className="hover:underline">
          <RelativeLink to={link}>{textContent}</RelativeLink>
        </Text>
      ) : (
        <Text>{textContent}</Text>
      )}
    </Flex>
  );

  return (
    <>
      {content}
      {!isLastItem && <FaAngleRight className="text-muted" />}
    </>
  );
}
