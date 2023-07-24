import React from 'react';

type AlertHeaderProps = {
  icon: React.ReactNode | React.ReactElement;
  title: string;
  description?: string;
};

export const AlertHeader: React.FC<AlertHeaderProps> = ({
  icon,
  title,
  description,
}) => {
  return (
    <div className="flex items-top p-md">
      <div className="text-yellow-500">{icon}</div>
      <div>
        <p className="text-lg font-semibold">{title}</p>
        {description && (
          <div className="overflow-y-auto max-h-[calc(100vh-14rem)]">
            <p className="m-0">{description}</p>
          </div>
        )}
      </div>
    </div>
  );
};
