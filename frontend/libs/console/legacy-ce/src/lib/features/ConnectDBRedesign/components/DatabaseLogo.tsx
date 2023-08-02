import React from 'react';
// import { MdSignalWifiStatusbarConnectedNoInternet1 } from 'react-icons/md';
import { Badge } from '../../../new-components/Badge';
import { IoCloudOfflineOutline } from 'react-icons/io5';

export const DatabaseLogo: React.FC<{
  title: string;
  image: string;
  releaseName?: string;
  noConnection: boolean;
}> = ({ title, image, releaseName, noConnection }) => {
  return (
    // adding pointer evens none just to make sure none of this captures clicks since that's handled in the parent for the radio buttons
    <div className="flex flex-col mt-2 items-center justify-center absolute h-full w-full pointer-events-none">
      <img
        src={image}
        className="h-[24px] mb-2 object-contain"
        alt={`${title} logo`}
      />
      <div className="text-black text-base">{title}</div>

      {noConnection ? (
        <div className="absolute top-0 right-0 m-3 ">
          <IoCloudOfflineOutline size={20} className="text-red-500" />
        </div>
      ) : (
        releaseName &&
        releaseName !== 'GA' && (
          <div className="absolute top-0 right-0 m-1 scale-75">
            <Badge color="indigo">{releaseName}</Badge>
          </div>
        )
      )}
    </div>
  );
};
