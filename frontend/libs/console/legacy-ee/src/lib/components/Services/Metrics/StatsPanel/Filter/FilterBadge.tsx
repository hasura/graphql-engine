import React from 'react';
import styles from '../../Metrics.module.scss';
import cross from '../../images/x-circle-metrics.svg';
import { Flex } from '@radix-ui/themes';

const FilterBadge = (props) => {
  const { onClick, text } = props;
  return (
    <Flex className={styles['filterBadge']} align="center">
      {text} <img onClick={onClick} src={cross} alt="Cross" />
    </Flex>
  );
};

export default FilterBadge;
