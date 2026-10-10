import CheckBox from '../CheckBox';
import styles from '../../Metrics.module.scss';
import { Grid } from '@radix-ui/themes';

const GenerateGroupBy = ({ id, title, value, onChange, checked }) => {
  const onCheckBoxClick = (e) => {
    const val = e.target.getAttribute('data-field-value');
    onChange(val);
  };
  return (
    <CheckBox
      id={id}
      onChange={onCheckBoxClick}
      title={title}
      value={value}
      checked={checked}
    />
  );
};
const GenerateGroupBys = ({ getTitle, values, selected, onChange }) => {
  const groupBy = values.map((v, i) => (
    <GenerateGroupBy
      key={i}
      id={`groupBy-${v}`}
      title={getTitle(v)}
      value={v}
      onChange={onChange}
      checked={selected.indexOf(v) !== -1}
    />
  ));
  return (
    <div className={styles['filterSectionWrapper']}>
      <Grid
        align="center"
        gapX="4"
        columns={{
          initial: '1',
          sm: '2',
          md: '4',
        }}
      >
        {groupBy}
      </Grid>
    </div>
  );
};

export default GenerateGroupBys;
