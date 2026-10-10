import React, { ReactElement } from 'react';
import { Analytics } from '@hasura/shared/analytics';
import { Flex, Link } from '@radix-ui/themes';

type Props = {
  label: string;
  icon: string | ReactElement<any>;
  url: string;
  id: string;
};

export function ListItem(props: Props) {
  const { label, icon, url, id } = props;
  return (
    <Analytics name={`ee-benefits-${id}-link`}>
      <Flex align="center" gap="2">
        {typeof icon === 'string' ? (
          <img className="h-7 w-7" src={icon} alt={label} />
        ) : (
          <div className="text-muted">{icon}</div>
        )}
        <Link
          href={url}
          color="gray"
          underline="none"
          target="_blank"
          rel="noopener noreferrer"
          size="2"
        >
          {label}
        </Link>
      </Flex>
    </Analytics>
  );
}
