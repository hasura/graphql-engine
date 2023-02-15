import clsx from 'clsx';
import React from 'react';

const twStyles = {
  container: `border border-gray-300 mt-3 shadow-md rounded bg-white p-6`,
  blueBorder: `border-l-4 border-l-[#297393]`,
};

export const InformationCard: React.FC<{
  blueLeftBorder?: boolean;
  className?: string;
  innerContainerClassName?: string;
}> = ({ children, blueLeftBorder, className, innerContainerClassName }) => {
  return (
    <div
      className={clsx(
        twStyles.container,
        blueLeftBorder && twStyles.blueBorder,
        className
      )}
    >
      {children}
    </div>
  );
};
