import { expect } from '@storybook/jest';
import { waitFor, within } from '@storybook/testing-library';
import { TestIds } from './ReactQueryStatusUI';

/**
 * This function checks that the elements in the displayed[] array are in the document,
 * And it checks that the elements in the notDisplayed[] array are NOT in the document
 * If no elements are supplied to notDisplayed[] the default is to check that no other Status elements are displayed
 * So, an XOR relationships between the displayed elements and the remaining elements in TestIds (status elements)
 *
 * In short, you can pass in a list of the elements you want to be in the document and it will, by default, make sure the other elements are NOT in the document
 */
export const checkForStatusElements = async ({
  displayed,
  notDisplayed,
  canvasElement,
  childContentId = TestIds.childContent,
}: {
  displayed: string[];
  notDisplayed?: string[];
  canvasElement: HTMLElement;
  childContentId?: string;
}) => {
  const c = within(canvasElement);

  for (const id of displayed) {
    await expect(await c.findByTestId(id)).toBeInTheDocument();
  }

  // if not explicity told, infer it
  const _notDisplayed =
    notDisplayed ??
    Object.entries({ ...TestIds, childContent: childContentId })
      .filter(([, id]) => !displayed.includes(id))
      .map(([, id]) => id);

  for (const id of _notDisplayed) {
    await expect(c.queryByTestId(id)).not.toBeInTheDocument();
  }
};

// these are for stories that implement the providers so they can wait for overlays to be gone for interactions to continue
export const waitForSpinnerOverlay = async (
  canvasElement: HTMLElement,
  timeout = 2000
) => {
  const canvas = within(canvasElement);

  return waitFor(
    async () =>
      expect(canvas.queryByTestId(TestIds.spinner)).not.toBeInTheDocument(),
    { timeout }
  );
};
export const waitForSkeletonOverlay = async (
  canvasElement: HTMLElement,
  timeout = 2000
) => {
  const canvas = within(canvasElement);

  return waitFor(
    async () =>
      expect(canvas.queryByTestId(TestIds.skeleton)).not.toBeInTheDocument(),
    { timeout }
  );
};

export const shorterTextContent = `As absolute is by amounted repeated entirely ye returned. These
            ready timed enjoy might sir yet one since. Years drift never if
            could forty being no. On estimable dependent as suffering on my.
            Rank it long have sure in room what as he. Possession travelling
            sufficient yet our. Talked vanity looked in to. Gay perceive led
            believed endeavor. Rapturous no of estimable oh therefore direction
            up. Sons the ever not fine like eyes all sure.`;
export const longerTextContent = `As absolute is by amounted repeated entirely ye returned.
                  These ready timed enjoy might sir yet one since. Years drift
                  never if could forty being no. On estimable dependent as
                  suffering on my. Rank it long have sure in room what as he.
                  Possession travelling sufficient yet our. Talked vanity looked
                  in to. Gay perceive led believed endeavor. Rapturous no of
                  estimable oh therefore direction up. Sons the ever not fine
                  like eyes all sure. As absolute is by amounted repeated
                  entirely ye returned. These ready timed enjoy might sir yet
                  one since. Years drift never if could forty being no. On
                  estimable dependent as suffering on my. Rank it long have sure
                  in room what as he. Possession travelling sufficient yet our.
                  Talked vanity looked in to. Gay perceive led believed
                  endeavor. Rapturous no of estimable oh therefore direction up.
                  Sons the ever not fine like eyes all sure. As absolute is by
                  amounted repeated entirely ye returned. These ready timed
                  enjoy might sir yet one since. Years drift never if could
                  forty being no. On estimable dependent as suffering on my.
                  Rank it long have sure in room what as he. Possession
                  travelling sufficient yet our. Talked vanity looked in to. Gay
                  perceive led believed endeavor. Rapturous no of estimable oh
                  therefore direction up. Sons the ever not fine like eyes all
                  sure. As absolute is by amounted repeated entirely ye
                  returned. These ready timed enjoy might sir yet one since.
                  Years drift never if could forty being no. On estimable
                  dependent as suffering on my. Rank it long have sure in room
                  what as he. Possession travelling sufficient yet our. Talked
                  vanity looked in to. Gay perceive led believed endeavor.
                  Rapturous no of estimable oh therefore direction up. Sons the
                  ever not fine like eyes all sure.`;
