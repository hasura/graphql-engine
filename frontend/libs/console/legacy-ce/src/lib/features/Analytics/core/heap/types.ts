// -------------------------------------------------------------------------------------------------
// GLOBAL OBJECTS ----------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------
/**
 * The globally available window.heap object. It's available or not based if the server injects it or not.
 */
export interface GlobalWindowHeap {
  /**
   * The Heap's methods consumed by the app.
   *
   * You can check all the available methods here https://developers.heap.io/reference/client-side-apis-overview
   *
   * Consuming Heap is allowed only through the Analytics module, never directly.
   * @deprecated (when marked as deprecated, the IDE shows it as strikethrough'ed, helping the
   * developers realize that they should not use it)
   */
  heap?: {
    /**
     * @see: https://developers.heap.io/reference/add-user-properties
     */
    addUserProperties: (properties: Record<string, string>) => void;

    /**
     * Track custom events but the reserved ones.
     *
     * @see: https://developers.heap.io/reference/track
     *
     * Use trackCustomEvent to track custom events in Heap.
     * @deprecated (when marked as deprecated, the IDE shows it as strikethrough'ed, helping the
     * developers realize that they should not use it)
     */
    track: <EVENT_NAME extends string>(
      eventName: EVENT_NAME extends ReservedEventNames ? never : EVENT_NAME,
      properties?: Record<string, string>
    ) => void;
  };
}

// -------------------------------------------------------------------------------------------------
// DATA REDACTING ----------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------
export interface RedactOptions {
  redactText?: boolean;
  /**
   * A comma-separated list of HTML attributes to redact.
   *
   * Please note that ids cannot be fully redacted, be sure ids do not contain sensitive information.
   * @see https://developers.heap.io/docs/web#precise-data-redaction-via-heap-redact
   */
  htmlAttributesToRedact?: string;
}

export interface HtmlRedactAttributes {
  // ATTENTION: remember to update the ESLint react/forbid-dom-props rule too if you edit the attributes
  'data-heap-redact-text'?: boolean;
  'data-heap-redact-attributes'?: string;
}

// @see: https://developers.heap.io/reference/track
export type ReservedEventNames = 'click' | 'change' | 'submit';
