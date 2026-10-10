import React from 'react';
import { Flex } from '@radix-ui/themes';

type AlertHeaderProps = {
  icon: React.ReactNode | React.ReactElement<any>;
  title: string;
  description?: string;
};

export const AlertHeader: React.FC<AlertHeaderProps> = ({
  icon,
  title,
  description,
}) => {
  return (
    <Flex className="items-top p-4">
      <div className="text-yellow-500">{icon}</div>
      <div>
        <p className="text-lg font-semibold">{title}</p>
        {description && (
          <div className="overflow-y-auto max-h-[calc(100vh-14rem)]">
            <p className="m-0">{description}</p>
          </div>
        )}
      </div>
    </Flex>
  );
};
