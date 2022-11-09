import * as Sentry from '@sentry/react';

// Each context can be a a scalar or an object
type Context = Record<string, any>;

// context kind is defined as a flexible enum so that we stick to limited set of contexts
type ContextKind = 'debug' | string;

// Refer to https://docs.sentry.io/platforms/javascript/guides/react/enriching-events/context/
type Contexts = Record<ContextKind, Context>;

/*
	This function allows us to capture caught exceptions
	that we want the engineering team to be alerted about
*/
export function captureException(err: Error, contexts?: Contexts) {
  Sentry.captureException(err, { contexts });
}
