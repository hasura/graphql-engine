import React from 'react';

import { AlertMessageBox } from './styles';

// Alerts ***************************** //

export const Alerts = () => (
  <React.Fragment>
    <AlertMessageBox
      minWidth={1 / 2} // width ~ 50%
      height={1} // ~ theme.sizes[1]
      bg="green.light" // ~ theme.colors.green.light
      maxWidth={866} // ~ max-width: 866px
      borderLeft={4} // ~ theme.borders[4]
      borderColor="green.primary" // ~ theme.colors.green.primary
      borderRadius="xs" // ~ theme.radii.xs
      boxShadow={2} // ~ theme.shadows[2]
      my="lg" // margin-y-axis ~ theme.space.lg
    />
    <AlertMessageBox
      minWidth={1 / 2}
      height={1}
      bg="blue.light"
      maxWidth={866}
      borderLeft={4}
      borderColor="blue.primary"
      borderRadius="xs"
      boxShadow={2}
      my="lg"
    />
    <AlertMessageBox
      minWidth={1 / 2}
      height={1}
      bg="orange.light"
      maxWidth={866}
      borderLeft={4}
      borderColor="orange.primary"
      borderRadius="xs"
      boxShadow={2}
      my="lg"
    />
    <AlertMessageBox
      minWidth={1 / 2}
      height={1}
      bg="red.light"
      maxWidth={866}
      borderLeft={4}
      borderColor="red.primary"
      borderRadius="xs"
      boxShadow={2}
      my="lg"
    />
  </React.Fragment>
);
