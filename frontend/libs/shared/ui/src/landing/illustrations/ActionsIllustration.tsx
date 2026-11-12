import { HasuraIcon } from '../logo/HasuraIcon';
import { ACCENT, INK, IllustrationProps } from './primitives';

const GRAPHQL_PINK = '#ff3264';

/**
 * Actions landing illustration: a client calls Hasura over GraphQL, Hasura
 * exposes an Action that maps to a custom HTTP handler, and resolves data to/from
 * Postgres. Theme-aware re-draw of the former `common/img/actions.png`
 * (701×414), whose opaque-white surfaces and grey ink clashed with dark mode.
 */
export const ActionsIllustration = ({
  title = 'How Hasura Actions work',
  ...props
}: IllustrationProps) => (
  <svg
    viewBox="0 0 701 414"
    role="img"
    aria-label={title}
    xmlns="http://www.w3.org/2000/svg"
    {...props}
  >
    <title>{title}</title>

    {/* Client app (phone) */}
    <g>
      <rect
        x={10}
        y={44}
        width={58}
        height={104}
        rx={10}
        fill={INK.surface.fill}
        stroke={INK.line.stroke}
        strokeWidth={2.5}
        className={`${INK.surface.darkClass} ${INK.line.darkClass}`}
      />
      <line
        x1={30}
        y1={54}
        x2={48}
        y2={54}
        stroke={INK.line.stroke}
        strokeWidth={2.5}
        strokeLinecap="round"
        className={INK.line.darkClass}
      />
      <line
        x1={28}
        y1={138}
        x2={50}
        y2={138}
        stroke={INK.line.stroke}
        strokeWidth={2.5}
        strokeLinecap="round"
        className={INK.line.darkClass}
      />
      <text
        x={39}
        y={176}
        textAnchor="middle"
        fontSize={18}
        fontWeight={600}
        fill={INK.textStrong.fill}
        className={INK.textStrong.darkClass}
      >
        App
      </text>
    </g>

    {/* GraphQL hop: phone → Hasura */}
    <text
      x={157}
      y={46}
      textAnchor="middle"
      fontSize={18}
      fontWeight={600}
      fill={INK.textStrong.fill}
      className={INK.textStrong.darkClass}
    >
      GraphQL
    </text>
    {/* GraphQL mark */}
    <g>
      <polygon
        points="157,78 172,87 172,104 157,113 142,104 142,87"
        fill="none"
        stroke={GRAPHQL_PINK}
        strokeWidth={2}
      />
      <g fill={GRAPHQL_PINK}>
        <circle cx={157} cy={78} r={3} />
        <circle cx={172} cy={87} r={3} />
        <circle cx={172} cy={104} r={3} />
        <circle cx={157} cy={113} r={3} />
        <circle cx={142} cy={104} r={3} />
        <circle cx={142} cy={87} r={3} />
      </g>
    </g>
    <g
      stroke={INK.line.stroke}
      strokeWidth={2.5}
      className={INK.line.darkClass}
      fill="none"
    >
      <line x1={68} y1={95} x2={128} y2={95} strokeLinecap="round" />
      <line x1={186} y1={95} x2={246} y2={95} strokeLinecap="round" />
    </g>
    <polygon
      points="252,95 244,90 244,100"
      fill={INK.line.stroke}
      className="dark:fill-slate-400"
    />

    {/* Hasura container */}
    <rect
      x={252}
      y={12}
      width={200}
      height={140}
      rx={12}
      fill="none"
      stroke={INK.line.stroke}
      strokeWidth={2}
      strokeDasharray="5 5"
      className={INK.line.darkClass}
    />
    <HasuraIcon
      x={296}
      y={26}
      width={26}
      height={26}
      className="text-[#001934] dark:text-slate-100"
    />
    <text
      x={330}
      y={47}
      fontSize={21}
      fontWeight={700}
      letterSpacing={1}
      fill={INK.navy.fill}
      className={INK.navy.darkClass}
    >
      HASURA
    </text>
    {/* Actions node */}
    <rect
      x={276}
      y={72}
      width={152}
      height={46}
      rx={6}
      fill={INK.surface.fill}
      stroke={ACCENT}
      strokeWidth={2.5}
      className={INK.surface.darkClass}
    />
    <text
      x={352}
      y={96}
      textAnchor="middle"
      dominantBaseline="central"
      fontSize={19}
      fontWeight={700}
      fill={INK.navy.fill}
      className={INK.navy.darkClass}
    >
      Actions
    </text>

    {/* CRUD hop: Hasura → Postgres */}
    <text
      x={545}
      y={55}
      textAnchor="middle"
      fontSize={16}
      fontWeight={600}
      fill={INK.textStrong.fill}
      className={INK.textStrong.darkClass}
    >
      CRUD + realtime
    </text>
    <text
      x={545}
      y={76}
      textAnchor="middle"
      fontSize={16}
      fontWeight={600}
      fill={INK.textStrong.fill}
      className={INK.textStrong.darkClass}
    >
      With access control
    </text>
    <line
      x1={452}
      y1={95}
      x2={626}
      y2={95}
      stroke={INK.line.stroke}
      strokeWidth={2.5}
      strokeLinecap="round"
      className={INK.line.darkClass}
    />
    <polygon
      points="632,95 624,90 624,100"
      fill={INK.line.stroke}
      className="dark:fill-slate-400"
    />

    {/* Postgres cylinder */}
    <g
      fill="none"
      stroke={INK.line.stroke}
      strokeWidth={2.5}
      className={INK.line.darkClass}
    >
      <path d="M638,74 v42 a28,9 0 0 0 56,0 v-42" />
      <ellipse cx={666} cy={74} rx={28} ry={9} />
      <path d="M638,95 a28,9 0 0 0 56,0" />
    </g>
    <text
      x={666}
      y={150}
      textAnchor="middle"
      fontSize={16}
      fontWeight={600}
      fill={INK.text.fill}
      className={INK.text.darkClass}
    >
      Postgres
    </text>

    {/* Actions ↔ HTTP endpoint (bidirectional) */}
    <g stroke={ACCENT} strokeWidth={2.5} fill={ACCENT} strokeLinecap="round">
      <line x1={325} y1={128} x2={325} y2={230} />
      <polygon points="325,122 320,132 330,132" stroke="none" />
      <polygon points="325,236 320,226 330,226" stroke="none" />
      <line x1={379} y1={128} x2={379} y2={230} />
      <polygon points="379,122 374,132 384,132" stroke="none" />
      <polygon points="379,236 374,226 384,226" stroke="none" />
    </g>

    {/* HTTP endpoint */}
    <rect
      x={150}
      y={240}
      width={404}
      height={160}
      rx={12}
      fill="none"
      stroke={INK.line.stroke}
      strokeWidth={2}
      strokeDasharray="5 5"
      className={INK.line.darkClass}
    />
    <text
      x={352}
      y={270}
      textAnchor="middle"
      fontSize={18}
      fontWeight={700}
      fill={INK.textStrong.fill}
      className={INK.textStrong.darkClass}
    >
      HTTP endpoint
    </text>
    <rect
      x={184}
      y={288}
      width={336}
      height={96}
      rx={8}
      fill={INK.surface.fill}
      stroke={INK.line.stroke}
      strokeWidth={1.5}
      strokeDasharray="4 4"
      className={`${INK.surface.darkClass} ${INK.line.darkClass}`}
    />
    <g
      fontSize={15}
      fontWeight={600}
      fill={INK.textStrong.fill}
      className={INK.textStrong.darkClass}
    >
      <text x={206} y={316}>
        · Custom business logic
      </text>
      <text x={206} y={344}>
        · Data validation
      </text>
      <text x={206} y={372}>
        · Data enrichment and transformation
      </text>
    </g>
  </svg>
);
