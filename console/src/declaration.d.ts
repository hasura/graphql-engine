declare module '*.scss' {
  const content: { [className: string]: string };
  export default content;
}

declare module 'react-router-redux' {
  export const push: (path: string) => void;
}

declare module 'react-helmet' {
  const Helmet: React.FC<{ title: string }>;
  export default Helmet;
}

declare module 'react-cron-generator' {
  function onChange(value: string): void;

  type CronBuilderProps = {
    onChange: typeof onChange;
    value: string;
    showResultText?: boolean;
    showResultCron?: boolean;
  };
  const CronBuilder: React.FC<CronBuilderProps>;
  export { CronBuilderProps };
  export default CronBuilder;
}
declare namespace SvgPanZoom {
  export interface Instance {}
}

declare module 'react-bootstrap/lib/Tooltip';
declare module 'react-bootstrap/lib/Tabs';
declare module 'react-bootstrap/lib/OverlayTrigger';
declare module 'react-bootstrap/lib/Button';
declare module 'react-bootstrap/lib/InputGroup';
declare module 'react-bootstrap/lib/DropdownButton';
declare module 'react-bootstrap/lib/MenuItem';
declare module 'react-bootstrap/lib/Tab';
declare module 'react-bootstrap/lib/Modal';
