import styles from '../../Metrics.module.scss';

const FilterSection = ({ children }) => {
  return (
    <div className={styles['filterSectionWrapper']}>
      <div>{children}</div>
    </div>
  );
};
export default FilterSection;
