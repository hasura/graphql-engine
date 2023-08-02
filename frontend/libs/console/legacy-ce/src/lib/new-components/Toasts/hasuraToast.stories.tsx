import React from 'react';
import { StoryObj, Meta } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import AceEditor from 'react-ace';
import { within, userEvent } from '@storybook/testing-library';
import { expect } from '@storybook/jest';

import { hasuraToast } from '.';
import { Button } from '../Button';

export default {
  title: 'components/Toasts 🧬/API',
  parameters: {
    docs: {
      description: {
        component: `A component wrapping the \`react-hot-toast\` library to easily implement a toast system.`,
      },
      source: { type: 'code', state: 'open' },
    },
  },
  decorators: [Story => <div className="p-4 ">{Story()}</div>],
} as Meta<any>;

export const Basic: StoryObj<any> = {
  render: () => {
    return (
      <Button
        onClick={() =>
          hasuraToast({
            title: 'The toast title',
            message: 'The toast message',
          })
        }
      >
        <span>Add toast!</span>
      </Button>
    );
  },

  name: '🧰 Basic',

  parameters: {
    docs: {
      source: { state: 'open' },
    },
  },

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await userEvent.click(canvas.getByRole('button'));
    await expect(canvas.getByText('The toast title')).toBeInTheDocument();
  },
};

export const LongText: StoryObj<any> = {
  render: () => {
    return (
      <Button
        onClick={() =>
          hasuraToast({
            title:
              'Long notification with a long long long long long long long long long long long long long long long long long long long long long title',
            toastOptions: {
              duration: 10000000,
            },
            message: `
  In the vast expanse of the cosmos, stars twinkle like distant beacons of light, while galaxies swirl in magnificent cosmic ballets. The universe, with its awe-inspiring beauty and mysteries, has captivated the human imagination for millennia. From the ancient civilizations gazing at the night sky to the modern-day explorations of space, our quest to understand the cosmos has been a relentless pursuit.

  As we delve into the realms of astrophysics, we uncover the fundamental laws that govern the workings of the universe. The theories of general relativity and quantum mechanics provide us with profound insights into the nature of space, time, and matter. They reveal a cosmic tapestry woven with gravitational waves, black holes, and the dance of subatomic particles.

  The exploration of our own cosmic neighborhood, the Solar System, has been an ongoing endeavor. From the pioneering missions of the past to the cutting-edge technologies of today, we have ventured to the moon, sent rovers to Mars, and captured breathtaking images of the gas giants and icy moons that reside within our celestial backyard. Each discovery fuels our curiosity and propels us further into the unknown.

  Beyond our Solar System lie countless other star systems, each potentially harboring worlds of their own. The search for exoplanets, planets orbiting distant stars, has become a thriving field of research. Through telescopes and space probes, we have detected thousands of these alien worlds, some resembling our own, while others defy our understanding of what a planet can be. With every new discovery, we inch closer to answering the age-old question: Are we alone in the universe?

  But the cosmos is not only a realm of scientific inquiry; it also holds a profound cultural and philosophical significance. It has been a wellspring of inspiration for poets, artists, and thinkers throughout history. The celestial bodies have been intertwined with mythologies, shaping our understanding of the world and our place within it. The night sky, with its constellations and galaxies, invites us to ponder the mysteries of existence and contemplate our cosmic heritage.

  As we stand on the precipice of a new era, space exploration is poised to take a giant leap forward. Private companies are venturing into the realm of space tourism, promising ordinary individuals the chance to experience weightlessness and gaze upon the Earth from above. Meanwhile, international collaborations aim to establish permanent human settlements on other celestial bodies, with Mars being the prime candidate. These endeavors push the boundaries of human achievement and beckon us to embrace the vastness of the cosmos.

  In this grand cosmic symphony, we find ourselves at the intersection of science, imagination, and human curiosity. The universe, with all its wonders, invites us to explore, to dream, and to seek answers to the most profound questions of our existence. Whether we are peering through a telescope or contemplating the starry night sky, we are reminded of the infinite possibilities that lie beyond our earthly realm. So let us embark on this journey of discovery, with open minds and boundless curiosity, for the cosmos awaits us with its secrets and its splendor.`,
          })
        }
      >
        <span>Add toast!</span>
      </Button>
    );
  },

  name: '🧰 Long Text',

  parameters: {
    docs: {
      source: { state: 'open' },
    },
  },
};

export const VariantType: StoryObj<any> = {
  render: () => {
    return (
      <>
        <Button
          onClick={() =>
            hasuraToast({
              type: 'error',
              title: 'The toast title',
              message: 'The toast message',
            })
          }
        >
          <span>Add error toast!</span>
        </Button>
        <Button
          onClick={() =>
            hasuraToast({
              type: 'success',
              title: 'The toast title',
              message: 'The toast message',
            })
          }
        >
          <span>Add success toast!</span>
        </Button>
        <Button
          onClick={() =>
            hasuraToast({
              type: 'info',
              title: 'The toast title',
              message: 'The toast message',
            })
          }
        >
          <span>Add info toast!</span>
        </Button>
        <Button
          onClick={() =>
            hasuraToast({
              type: 'warning',
              title: 'The toast title',
              message: 'The toast message',
            })
          }
        >
          <span>Add warning toast!</span>
        </Button>
      </>
    );
  },

  name: '🎭 Variant - Type',

  parameters: {
    docs: {
      source: { state: 'open' },
    },
  },
};

export const VariantButton: StoryObj<any> = {
  render: () => {
    return (
      <>
        <Button
          onClick={() =>
            hasuraToast({
              type: 'error',
              title: 'The toast title',
              message: 'The toast message',
              button: {
                label: 'The toast button',
                onClick: action('Button clicked!'),
              },
            })
          }
        >
          <span>Add error toast!</span>
        </Button>
        <Button
          onClick={() =>
            hasuraToast({
              type: 'success',
              title: 'The toast title',
              message: 'The toast message',
              button: {
                label: 'The toast button',
                onClick: action('Button clicked!'),
              },
            })
          }
        >
          <span>Add success toast!</span>
        </Button>
        <Button
          onClick={() =>
            hasuraToast({
              type: 'info',
              title: 'The toast title',
              message: 'The toast message',
              button: {
                label: 'The toast button',
                onClick: action('Button clicked!'),
              },
            })
          }
        >
          <span>Add info toast!</span>
        </Button>
        <Button
          onClick={() =>
            hasuraToast({
              type: 'warning',
              title: 'The toast title',
              message: 'The toast message',
              button: {
                label: 'The toast button',
                onClick: action('Button clicked!'),
              },
            })
          }
        >
          <span>Add warning toast!</span>
        </Button>
      </>
    );
  },

  name: '🎭 Variant - Button',

  parameters: {
    docs: {
      source: { state: 'open' },
    },
  },
};

export const VariantChildren: StoryObj<any> = {
  render: () => {
    return (
      <>
        <Button
          onClick={() =>
            hasuraToast({
              type: 'error',
              title: 'The toast title',
              message: 'The toast message',
              children: (
                <div className="overflow-hidden">
                  Here is an embedded code:
                  <AceEditor
                    theme="github"
                    setOptions={{
                      minLines: 3,
                      maxLines: Infinity,
                      showGutter: false,
                      useWorker: false,
                    }}
                    value={`{
    "key": "value",
  }`}
                  />
                </div>
              ),
              toastOptions: {
                duration: 1000000,
              },
            })
          }
        >
          <span>Add error toast!</span>
        </Button>
        <Button
          onClick={() =>
            hasuraToast({
              type: 'success',
              title: 'The toast title',
              message: 'The toast message',
              children: (
                <div className="overflow-hidden">
                  Here is an embedded code:
                  <AceEditor
                    theme="github"
                    setOptions={{
                      minLines: 3,
                      maxLines: Infinity,
                      showGutter: false,
                      useWorker: false,
                    }}
                    value={`{
    "key": "value",
  }`}
                  />
                </div>
              ),
            })
          }
        >
          <span>Add success toast!</span>
        </Button>
        <Button
          onClick={() =>
            hasuraToast({
              type: 'info',
              title: 'The toast title',
              message: 'The toast message',
              children: (
                <div className="overflow-hidden">
                  Here is an embedded code:
                  <AceEditor
                    theme="github"
                    setOptions={{
                      minLines: 3,
                      maxLines: Infinity,
                      showGutter: false,
                      useWorker: false,
                    }}
                    value={`{
    "key": "value",
  }`}
                  />
                </div>
              ),
            })
          }
        >
          <span>Add info toast!</span>
        </Button>
        <Button
          onClick={() =>
            hasuraToast({
              type: 'warning',
              title: 'The toast title',
              message: 'The toast message',
              children: (
                <div className="overflow-hidden">
                  Here is an embedded code:
                  <AceEditor
                    theme="github"
                    setOptions={{
                      minLines: 3,
                      maxLines: Infinity,
                      showGutter: false,
                      useWorker: false,
                    }}
                    value={`{
    "key": "value",
  }`}
                  />
                </div>
              ),
            })
          }
        >
          <span>Add warning toast!</span>
        </Button>
      </>
    );
  },

  name: '🎭 Variant - Children',

  parameters: {
    docs: {
      source: { state: 'open' },
    },
  },
};

export const VariantToastOptions: StoryObj<any> = {
  render: () => {
    return (
      <>
        <Button
          onClick={() =>
            hasuraToast({
              type: 'error',
              title: 'The toast title',
              message: 'The toast message',
              toastOptions: {
                style: {
                  minWidth: 'min(350px, 75vw)',
                },
              },
            })
          }
        >
          <span>Add error toast!</span>
        </Button>
        <Button
          onClick={() =>
            hasuraToast({
              type: 'success',
              title: 'The toast title',
              message: 'The toast message',
              toastOptions: {
                style: {
                  minWidth: 'min(150px, 75vw)',
                },
              },
            })
          }
        >
          <span>Add success toast!</span>
        </Button>
        <Button
          onClick={() =>
            hasuraToast({
              type: 'info',
              title: 'The toast title',
              message: 'The toast message',
              toastOptions: {
                style: {
                  minWidth: 'min(250px, 75vw)',
                },
              },
            })
          }
        >
          <span>Add info toast!</span>
        </Button>
        <Button
          onClick={() =>
            hasuraToast({
              type: 'warning',
              title: 'The toast title',
              message: 'The toast message',
              toastOptions: {
                style: {
                  minWidth: 'min(300px, 75vw)',
                },
              },
            })
          }
        >
          <span>Add warning toast!</span>
        </Button>
      </>
    );
  },

  name: '🎭 Variant - Toast options',

  parameters: {
    docs: {
      source: { state: 'open' },
    },
  },
};
