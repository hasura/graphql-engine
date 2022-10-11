import { useEffect, useRef } from 'react';

export const useSyncBackToReduxState = (
  onDriverChange: (driver: string, name: string) => void,
  name: string,
  driver: string
) => {
  const refs = useRef({ name, onDriverChange });

  refs.current = {
    name,
    onDriverChange,
  };

  /**
   * Why use this hook?
   *
   * Name input fields are different across the old and the new UI but we want to share the value between them
   * even when the forms themselves get changed from the old/new when driver changes. This means that the
   * the `name` and the `onDriverChange` function to update the legacy UI's reducer remain the same across any change in
   * their respective values.
   *
   * Only during a change of `driver` do we need to update the legacy UI's reducer with the latest value of `name` before the
   * current UI gets destroyed.
   */
  useEffect(() => {
    refs.current.onDriverChange(driver, refs.current.name);
  }, [driver]);
};
