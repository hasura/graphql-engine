declare module '*.scss' {
  const content: { [className: string]: string };
  export default content;
}

declare namespace SvgPanZoom {
  export interface Instance {}
}

declare namespace React {
  interface HTMLAttributes<T> extends DOMAttributes<T> {
    css?: import('styled-components').CSSProp;
  }
}
