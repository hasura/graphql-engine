import GenerateGroupBys from './GenerateGroupBys';
import styles from '../../Metrics.module.scss';
import { Button } from '@hasura/shared/ui';

const GroupBys = ({ getTitle, reset, selected, values, onChange }) => {
  const renderSelectedGroupCount = () => {
    return selected.length > 0 ? `(${selected.length})` : '';
  };
  const resetGroup = () => {
    if (selected.length > 0) {
      return (
        <Button size="1" mode="destructive" onClick={reset}>
          Remove all group by
        </Button>
      );
    }
    return null;
  };
  return (
    <div className="groupByElementWrapper">
      <div className={styles['filterBtnWrapper']}>
        <div className={styles['subHeader']}>
          Group By {renderSelectedGroupCount()}
        </div>
        {resetGroup()}
      </div>
      <GenerateGroupBys
        getTitle={getTitle}
        selected={selected}
        values={values}
        onChange={onChange}
      />
    </div>
  );
};

export default GroupBys;
