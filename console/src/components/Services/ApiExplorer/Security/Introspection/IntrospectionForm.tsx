import React, { useState, useEffect } from 'react';
import { useDispatch } from 'react-redux';
import { Button } from '../../../../Common';
import { TableFormProps } from '../../../../Common/Table';
import styles from '../Security.scss';
import { updateIntrospectionOptions } from '../actions';

const IntrospectionForm: React.FC<
  TableFormProps<{ roleName: string; instrospectionIsDisabled: boolean }>
> = ({ collapseForm, currentData, currentRowKey }) => {
  const [formState, setFormState] = useState<{
    roleName: string;
    instrospectionIsDisabled: boolean;
  }>({
    roleName: currentRowKey,
    instrospectionIsDisabled: currentData.instrospectionIsDisabled ?? true,
  });

  const dispatch = useDispatch();

  useEffect(() => {
    setFormState({
      roleName: currentRowKey,
      instrospectionIsDisabled: currentData.instrospectionIsDisabled,
    });
  }, [currentData]);

  const submit = (e: React.FormEvent<HTMLFormElement>) => {
    e.preventDefault();

    dispatch(
      updateIntrospectionOptions({
        roleName: formState.roleName,
        introspectionIsDisabled: formState.instrospectionIsDisabled,
        callback: () => collapseForm(),
      })
    );
  };

  const onRadioChange = (state: boolean) => () => {
    setFormState({
      roleName: currentRowKey,
      instrospectionIsDisabled: state,
    });
  };

  return (
    <section className={styles.form_section}>
      <div className={styles.top}>
        <Button size="xs" onClick={() => collapseForm()}>
          Close
        </Button>
        <h5>
          {currentRowKey === 'global'
            ? 'Global Settings'
            : `Role: ${currentRowKey}`}
        </h5>
      </div>
      <form onSubmit={submit}>
        <div
          key={currentRowKey}
          className={`${styles.form_group} ${styles.form_justify}`}
        >
          <div className={styles.left}>
            <h5>Introspection</h5>
            <p>Enable GraphQL schema introspection requests.</p>
          </div>
          <div className={styles.right}>
            <div className={styles.radio_group}>
              <div className="radio_input">
                <input
                  type="radio"
                  className="legacy-input-fix"
                  id="enable_introspection"
                  checked={!formState.instrospectionIsDisabled}
                  onChange={onRadioChange(false)}
                />
                <label htmlFor="enable_introspection">Enabled</label>
              </div>

              <div className="radio_input">
                <input
                  type="radio"
                  className="legacy-input-fix"
                  id="disable_introspection"
                  checked={!!formState.instrospectionIsDisabled}
                  onChange={onRadioChange(true)}
                />
                <label htmlFor="disable_introspection">Disabled</label>
              </div>
            </div>
          </div>
        </div>

        <div className="submit_btn">
          <Button size="xs" color="yellow" type="submit">
            Save Settings
          </Button>
        </div>
      </form>
    </section>
  );
};

export default IntrospectionForm;
