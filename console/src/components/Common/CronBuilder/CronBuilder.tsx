import React from 'react';
import CronBuilder, { CronBuilderProps } from 'react-cron-generator';
import 'react-cron-generator/dist/cron-builder.css';
import './CronBuilder.css';

const Builder = (props: CronBuilderProps) => {
  return <CronBuilder {...props} />;
};

export default Builder;
