/// <reference types="@docusaurus/module-type-aliases" />

type TrackOptions = {
  label: string;
  action: string;
  category: string;
  path: string;
  placement?: string;
  cta?: string;
  screen_size?: string;
}

interface Window {
  analytics: {
    page: (title: string) => void,
    track: <A>(eventName: string, options: A | TrackOptions) => void,
   };
}