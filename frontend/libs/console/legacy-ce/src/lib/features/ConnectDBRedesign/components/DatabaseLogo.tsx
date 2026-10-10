import React from 'react';
// import { MdSignalWifiStatusbarConnectedNoInternet1 } from 'react-icons/md';
import { Badge, Text } from '@hasura/shared/ui';
import { IoCloudOfflineOutline } from 'react-icons/io5';
import { Flex } from '@radix-ui/themes';

export const DatabaseLogo: React.FC<{
  title: string;
  image: string;
  releaseName?: string;
  noConnection: boolean;
}> = ({ title, image, releaseName, noConnection }) => {
  return (
    // adding pointer evens none just to make sure none of this captures clicks since that's handled in the parent for the radio buttons
    <Flex
      direction="column"
      align="center"
      justify="center"
      className="mt-2 absolute h-full w-full pointer-events-none"
    >
      <img
        src={image}
        className="h-[24px] mb-2 object-contain"
        alt={`${title} logo`}
      />
      <Text>{title}</Text>

      {noConnection ? (
        <div className="absolute top-0 right-0 m-3">
          <Text color="red">
            <IoCloudOfflineOutline size={20} />
          </Text>
        </div>
      ) : (
        releaseName &&
        releaseName !== 'GA' && (
          <div className="absolute top-0 right-0 m-1 scale-75">
            <Badge color="indigo">{releaseName}</Badge>
          </div>
        )
      )}
    </Flex>
  );
};
