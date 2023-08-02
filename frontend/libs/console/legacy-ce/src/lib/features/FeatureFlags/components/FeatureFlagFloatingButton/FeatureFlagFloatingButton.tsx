import React from 'react';
import { Button } from '../../../../new-components/Button';
import { FaFlask } from 'react-icons/fa';
import { useDispatch } from 'react-redux';
import _push from '../../../../components/Services/Data/push';

export const FeatureFlagFloatingButton = () => {
  const dispatch = useDispatch();
  return (
    <Button
      icon={<FaFlask />}
      className="fixed flex items-center justify-center bottom-4 right-4"
      onClick={() => dispatch(_push('/settings/feature-flags'))}
    />
  );
};
