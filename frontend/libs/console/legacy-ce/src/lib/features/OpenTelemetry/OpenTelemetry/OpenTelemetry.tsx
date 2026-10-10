import type { FormValues } from './components/Form/schema';
import { defaultValues } from './components/Form/schema';

import { Form } from './components/Form/Form';
import { Header } from './components/Header/Header';
import { EELiteAccessStatus, EETrialCard } from '../../EETrial';
import { Flex } from '@radix-ui/themes';
import { Text } from '@hasura/shared/ui';

interface OpenTelemetryProps {
  skeletonMode: boolean;
  isFirstTimeSetup: boolean;
  metadataFormValues: FormValues;
  withoutLicense?: boolean;
  setOpenTelemetry: (formValues: FormValues) => Promise<void>;
  eeAccess?: EELiteAccessStatus;
  loading?: boolean;
}

/**
 * All the OpenTelemetry page visual elements.
 */
export function OpenTelemetry(props: OpenTelemetryProps) {
  const {
    skeletonMode,
    isFirstTimeSetup,
    metadataFormValues,
    withoutLicense = false,
    setOpenTelemetry,
    eeAccess,
    loading = false,
  } = props;

  const formValues = metadataFormValues || defaultValues;

  let headerMode: 'enabled' | 'disabled' | 'skeleton' = 'enabled';

  if (!metadataFormValues || metadataFormValues.status !== 'enabled')
    headerMode = 'disabled';

  if (withoutLicense) headerMode = 'disabled';

  if (skeletonMode) headerMode = 'skeleton';

  return (
    <Flex
      gap="4"
      direction="column"
      className="space-y-md max-w-(--breakpoint-lg) p-4"
    >
      {/*
        While the form is stateful and shows its own version of the OpenTelemetry config, the
        Header reflects the real OpenTelemetry config stored in the metadata. It means that when
        the users enable OpenTelemetry through the form toggle, the Header will show the "disabled"
        status because OpenTelemetry is not enabled until the users submit the form. Once submitted,
        and the metadata is invalidated and reloaded, then the Header will reflect the "enabled"
        status.
        */}
      <Header mode={headerMode} />

      {withoutLicense ? (
        <EETrialCard
          cardTitle="Gain end-to-end visibility and performance insights with OpenTelemetry exports"
          id="open-telemetry"
          cardText={
            <Text>
              Collect, aggregate and export metrics data from your API to your
              APM provider to give you a view of your systems performance to
              help troubleshoot issues.
            </Text>
          }
          buttonLabel="Enable Enterprise"
          eeAccess={eeAccess}
          horizontal
        />
      ) : (
        <div>
          {/* This div avoid space-y-md separating too much the toggle and the form */}
          <Form
            onSubmit={setOpenTelemetry}
            defaultValues={formValues}
            skeletonMode={skeletonMode}
            firstTimeSetup={isFirstTimeSetup}
            loading={loading}
          />
        </div>
      )}
    </Flex>
  );
}
