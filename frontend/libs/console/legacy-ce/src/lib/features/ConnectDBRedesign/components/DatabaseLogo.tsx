import React from 'react';

export const DatabaseLogo: React.FC<{ title: string; image: string }> = ({
  title,
  image,
}) => {
  return (
    <div className="flex flex-col mt-2 items-center">
      <img
        src={image}
        className="h-[16px] w-[16px] mb-2"
        alt={`${title} logo`}
      />
      <div className="text-black text-base">{title}</div>
    </div>
  );
};
