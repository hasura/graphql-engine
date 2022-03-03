/// <reference types="@docusaurus/module-type-aliases" />

type TrackOptions = {
  label: string;
  action: string;
  category: string;
  placement: string;
  cta: string;
  screen_size: string;
  page: string;
}

interface Window {
  analytics: {
    page: (title: string) => void,
    track: (eventName: string, options: TrackOptions) => void,
   };
}