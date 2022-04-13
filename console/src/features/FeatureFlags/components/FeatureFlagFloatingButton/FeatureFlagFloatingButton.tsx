import React from 'react';
import { FaFlask } from 'react-icons/fa';
import { useDispatch } from 'react-redux';
import _push from '../../../../components/Services/Data/push';

export const FeatureFlagFloatingButton = () => {
  const dispatch = useDispatch();
  return (
    <button
      onClick={() => dispatch(_push('/settings/feature-flags'))}
      className="fixed flex items-center justify-center bottom-4 right-4 bg-white border overflow-hidden shadow-xl rounded-lg font-sans w-12 h-12"
    >
      <FaFlask />
    </button>
  );
};
