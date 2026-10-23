import {
  ACCENT,
  EngineWithOutputs,
  IllustrationProps,
  AccentArrow,
} from './primitives';

/**
 * Cron Triggers landing illustration: a calendar (schedule) drives the
 * "Hasura events" engine, which fans out to three delivery targets. Theme-aware
 * re-draw of the former `common/img/cron-trigger.png` (1131×368).
 */
export const CronTriggerIllustration = ({
  title = 'How Hasura Cron Triggers work',
  ...props
}: IllustrationProps) => (
  <svg
    viewBox="0 0 1131 368"
    role="img"
    aria-label={title}
    xmlns="http://www.w3.org/2000/svg"
    {...props}
  >
    <title>{title}</title>

    {/* Calendar (schedule) icon */}
    <g fill="none" stroke={ACCENT} strokeWidth={5} strokeLinecap="round">
      {/* binding tabs */}
      <line x1={58} y1={138} x2={58} y2={158} />
      <line x1={122} y1={138} x2={122} y2={158} />
      {/* body */}
      <rect x={32} y={150} width={116} height={82} rx={8} />
      {/* header rule */}
      <line x1={32} y1={176} x2={148} y2={176} strokeWidth={4} />
      {/* day grid */}
      <g strokeWidth={3}>
        <line x1={71} y1={176} x2={71} y2={232} />
        <line x1={109} y1={176} x2={109} y2={232} />
        <line x1={32} y1={204} x2={148} y2={204} />
      </g>
    </g>

    {/* Calendar → Hasura events */}
    <AccentArrow points="160,183 312,183" headAt={[312, 183]} headDir="right" />

    {/* The engine + its three delivery targets */}
    <EngineWithOutputs boxX={314} outX={810} />
  </svg>
);
