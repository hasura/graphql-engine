import { IndicatorCard, InputField, Text } from '@hasura/shared/ui';
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
    <div className="mt-2">
      {neonDBURL ? (
        <IndicatorCard status="positive" showIcon className="mb-1">
          <Text>Neon Database created successfully!</Text>
        </IndicatorCard>
      ) : (
        <>
          {showNeonButton ? (
            <NeonButton neonButtonProps={neonButtonProps} dbEnvVar={dbEnvVar} />
          ) : (
            <InputField
              name={dbEnvVar.Name}
              fieldProps={{
                placeholder: dbEnvVar.Name,
              }}
            />
          )}
        </>
      )}
    </div>
  );
}
