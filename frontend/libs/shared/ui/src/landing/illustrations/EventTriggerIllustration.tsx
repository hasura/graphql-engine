import {
  ACCENT,
  EngineWithOutputs,
  INK,
  IllustrationProps,
  OutputBox,
  AccentArrow,
} from './primitives';

/**
 * Event Triggers landing illustration: three event sources → Postgres → the
 * "Hasura events" engine → three delivery targets. Theme-aware re-draw of the
 * former `common/img/event-trigger.png` (1516×368).
 */
export const EventTriggerIllustration = ({
  title = 'How Hasura Event Triggers work',
  ...props
}: IllustrationProps) => (
  <svg
    viewBox="0 0 1516 368"
    role="img"
    aria-label={title}
    xmlns="http://www.w3.org/2000/svg"
    {...props}
  >
    <title>{title}</title>

    {/* Sources (left column) */}
    <OutputBox x={2} y={2} width={314} height={82} label="APIs" />
    <OutputBox x={2} y={142} width={314} height={82} label="Background jobs" />
    <OutputBox
      x={2}
      y={282}
      width={314}
      height={82}
      label="GraphQL mutations"
    />

    {/* Sources converge into Postgres */}
    <polyline
      points="318,43 505,183"
      fill="none"
      stroke={ACCENT}
      strokeWidth={2.5}
      strokeLinecap="round"
    />
    <polyline
      points="318,323 505,183"
      fill="none"
      stroke={ACCENT}
      strokeWidth={2.5}
      strokeLinecap="round"
    />
    <AccentArrow points="318,183 522,183" headAt={[522, 183]} headDir="right" />

    {/* Postgres cylinder */}
    <text
      x={570}
      y={98}
      textAnchor="middle"
      fontSize={22}
      fontWeight={600}
      fill={INK.text.fill}
      className={INK.text.darkClass}
    >
      Postgres
    </text>
    <g
      fill="none"
      stroke={INK.line.stroke}
      strokeWidth={2.5}
      className={INK.line.darkClass}
    >
      <path d="M525,128 v110 a45,14 0 0 0 90,0 v-110" />
      <ellipse cx={570} cy={128} rx={45} ry={14} />
      <path d="M525,165 a45,14 0 0 0 90,0" />
      <path d="M525,202 a45,14 0 0 0 90,0" />
    </g>

    {/* Postgres → Hasura events */}
    <AccentArrow points="617,183 698,183" headAt={[698, 183]} headDir="right" />

    {/* The engine + its three delivery targets */}
    <EngineWithOutputs boxX={700} boxWidth={312} outX={1196} />
  </svg>
);
