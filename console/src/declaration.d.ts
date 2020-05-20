declare module '*.scss' {
  const content: { [className: string]: string };
  export default content;
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
