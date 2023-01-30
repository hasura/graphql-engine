import type { UseFormReset } from 'react-hook-form';
import { useRef, useEffect } from 'react';

import type { FormValues } from '../schema';

type Params = {
  skeletonMode: boolean;
  defaultValues: FormValues;
  reset: UseFormReset<FormValues>;
};

/**
 * During the first metadata loading (aka when skeletonMode is true) the form is rendered but
 * covered by the skeletons. When the skeletons disappear, the metadata-based form values must take
 * place of the default (fake) ones. This could happen only through a form reset.
 *
 * Please note: this hook takes for granted that passing from skeletonMode=true to false means that
 * the metadata is loaded. Also, at the time of writing, the skeletonMode never returns true anymore.
 */
export function useResetDefaultFormValues(params: Params) {
  const { reset, defaultValues, skeletonMode } = params;

  const latestParams = useRef({ defaultValues, reset });
  latestParams.current = { defaultValues, reset };

  const prevSkeletonMode = useRef(skeletonMode);

  // Detect skeletonMode passing from true to false
  useEffect(() => {
    const wasInSkeletonMode = prevSkeletonMode.current;
    const skeletonModeEnded = !skeletonMode;

    const metadataLoadingIsCompleted = wasInSkeletonMode && skeletonModeEnded;

    if (metadataLoadingIsCompleted) {
      latestParams.current.reset(latestParams.current.defaultValues);
    }
  }, [skeletonMode]);

  // The order matters! Updating prevSkeletonMode must happen after consuming it!
  useEffect(() => {
    prevSkeletonMode.current = skeletonMode;
  }, [skeletonMode]);
}
