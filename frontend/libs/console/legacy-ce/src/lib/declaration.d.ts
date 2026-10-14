declare module '*.scss' {
  const content: { [className: string]: string };
  export default content;
}

declare namespace SvgPanZoom {
  export type Instance = Record<string, never>;
}

declare module '*.svg' {
  const content: string;
  export default content;
}

declare module '*.png';
