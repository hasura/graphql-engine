import { InputField } from '@/new-components/Form';
import React from 'react';
import { FaCheckCircle } from 'react-icons/fa';
import { NeonButtonProps } from '../../types';
import { NeonButton } from './NeonButton';
import { RequiredEnvVar } from '../../../../types';

type InputWrapperProps = {
  neonDBURL: string;
  showNeonButton: boolean;
  neonButtonProps: NeonButtonProps;
  dbEnvVar: RequiredEnvVar;
};

export function InputWrapper(props: InputWrapperProps) {
  const { neonDBURL, showNeonButton, neonButtonProps, dbEnvVar } = props;

  return (
    <>
      {neonDBURL ? (
        <div className="flex justify-start items-center mt-md mb-lg">
          <FaCheckCircle className="text-green-500 mr-2" />
          <span>Neon Database created succesfully!</span>
        </div>
      ) : (
        <>
          {showNeonButton ? (
            <NeonButton neonButtonProps={neonButtonProps} dbEnvVar={dbEnvVar} />
          ) : (
            <InputField name={dbEnvVar.Name} placeholder={dbEnvVar.Name} />
          )}
        </>
      )}
    </>
  );
}
