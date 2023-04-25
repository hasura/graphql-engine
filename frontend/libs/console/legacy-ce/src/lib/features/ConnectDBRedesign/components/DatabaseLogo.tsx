import React from 'react';
import { Badge } from '../../../new-components/Badge';

export const DatabaseLogo: React.FC<{
  title: string;
  image: string;
  releaseName?: string;
}> = ({ title, image, releaseName }) => {
  return (
    // adding pointer evens none just to make sure none of this captures clicks since that's handled in the parent for the radio buttons
    <div className="flex flex-col mt-2 items-center justify-center absolute h-full w-full pointer-events-none">
      <img
        src={image}
        className="h-[24px] mb-2 object-contain"
        alt={`${title} logo`}
      />
      <div className="text-black text-base">{title}</div>
      {releaseName && releaseName !== 'GA' && (
        <div className="absolute top-0 right-0 m-1 scale-75">
          <Badge color="indigo">{releaseName}</Badge>
        </div>
      )}
    </div>
  );
};
