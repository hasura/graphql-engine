import {
  ACCENT,
  EngineWithOutputs,
  IllustrationProps,
  AccentArrow,
} from './primitives';

/**
 * One-off / Scheduled events landing illustration: a clock (point in time)
 * drives the "Hasura events" engine, which fans out to three delivery targets.
 * Theme-aware re-draw of the former `common/img/scheduled-event.png` (1182×368).
 */
export const ScheduledEventIllustration = ({
  title = 'How Hasura scheduled events work',
  ...props
}: IllustrationProps) => (
  <svg
    viewBox="0 0 1182 368"
    role="img"
    aria-label={title}
    xmlns="http://www.w3.org/2000/svg"
    {...props}
  >
    <title>{title}</title>

    {/* Clock (point-in-time) icon */}
    <g fill="none" stroke={ACCENT} strokeLinecap="round">
      <circle cx={120} cy={183} r={82} strokeWidth={6} />
      {/* hands */}
      <line x1={120} y1={183} x2={120} y2={123} strokeWidth={6} />
      <line x1={120} y1={183} x2={162} y2={214} strokeWidth={6} />
      {/* hour ticks */}
      <g strokeWidth={5}>
        <line x1={120} y1={111} x2={120} y2={118} />
        <line x1={120} y1={248} x2={120} y2={255} />
        <line x1={48} y1={183} x2={55} y2={183} />
        <line x1={185} y1={183} x2={192} y2={183} />
      </g>
    </g>

    {/* Clock → Hasura events */}
    <AccentArrow points="205,183 363,183" headAt={[363, 183]} headDir="right" />

    {/* The engine + its three delivery targets */}
    <EngineWithOutputs boxX={365} outX={862} />
  </svg>
);
