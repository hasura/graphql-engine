import { SVGProps } from 'react';

/**
 * Shared colour + geometry primitives for the landing-page "architecture"
 * illustrations (Actions, Event Triggers, Cron Triggers, One-off Scheduled
 * events).
 *
 * These illustrations used to ship as flat PNGs (`common/img/*.png`). The PNGs
 * were baked for a light background — opaque-white surfaces and mid-grey text /
 * outlines — so once the console gained a dark appearance they stood out as
 * glaring light artefacts on a dark page. Re-authoring them as inline SVG lets
 * every surface, line and label react to the appearance the same way the rest
 * of the console does.
 *
 * Theme mechanism (see `libs/shared/ui/src/theme`): the ThemeProvider toggles a
 * `light` / `dark` class on <html>; Tailwind's `dark:` variant is wired to that
 * class (`@custom-variant dark (&:where(.dark, .dark *))`). We lean on that
 * single mechanism here:
 *
 *   - the LIGHT colour is a plain SVG presentation attribute (`fill` / `stroke`),
 *     so it renders identically to the old PNGs even with zero CSS applied — this
 *     is what guarantees no light-mode regression;
 *   - the DARK colour is a single `dark:` utility class. A CSS declaration beats
 *     an SVG presentation attribute, so under `.dark` the utility wins and the
 *     element brightens; in light mode the utility simply doesn't match.
 *
 * The brand orange (#ff9a00) reads well on both backgrounds, so accent marks
 * carry no `dark:` override and stay constant across appearances.
 */

/** Brand orange used for every accent (arrows, the "Hasura events" box, icons). */
export const ACCENT = '#ff9a00';

/**
 * Light presentation colour + the matching `dark:` utility for each semantic
 * role. Light values mirror the original PNG palette exactly.
 */
export const INK = {
  /** Primary diagram labels (the mid-grey text in the PNGs). */
  text: { fill: '#909090', darkClass: 'dark:fill-slate-300' },
  /** Darker label text used by the Actions diagram. */
  textStrong: { fill: '#707070', darkClass: 'dark:fill-slate-300' },
  /** Thin box outlines (the light-grey rounded rectangles). */
  outline: { stroke: '#d0d0d0', darkClass: 'dark:stroke-slate-600' },
  /** Grey connector lines / arrow heads. */
  line: { stroke: '#909090', darkClass: 'dark:stroke-slate-400' },
  /** Opaque white surfaces → a muted dark card in dark mode. */
  surface: { fill: '#ffffff', darkClass: 'dark:fill-slate-800' },
  /** The navy Hasura wordmark / in-box labels. */
  navy: { fill: '#001934', darkClass: 'dark:fill-slate-100' },
} as const;

type BoxProps = {
  x: number;
  y: number;
  width: number;
  height: number;
  label: string;
  /** Font size for the label, defaults to 22. */
  fontSize?: number;
};

/**
 * An outlined rounded rectangle with a centred grey label — the repeated
 * "source" / "sink" node shared by the three Eventing diagrams.
 */
export const OutputBox = ({
  x,
  y,
  width,
  height,
  label,
  fontSize = 22,
}: BoxProps) => (
  <g>
    <rect
      x={x}
      y={y}
      width={width}
      height={height}
      rx={10}
      fill="none"
      stroke={INK.outline.stroke}
      strokeWidth={2.5}
      className={INK.outline.darkClass}
    />
    <text
      x={x + width / 2}
      y={y + height / 2}
      textAnchor="middle"
      dominantBaseline="central"
      fontSize={fontSize}
      fontWeight={600}
      fill={INK.text.fill}
      className={INK.text.darkClass}
    >
      {label}
    </text>
  </g>
);

/**
 * The central orange "Hasura events" node. Orange on both appearances, so no
 * `dark:` override is needed.
 */
export const HasuraEventsBox = ({
  x,
  y,
  width,
  height,
}: Omit<BoxProps, 'label' | 'fontSize'>) => (
  <g>
    <rect
      x={x}
      y={y}
      width={width}
      height={height}
      rx={10}
      fill="none"
      stroke={ACCENT}
      strokeWidth={4}
    />
    <text
      x={x + width / 2}
      y={y + height / 2}
      textAnchor="middle"
      dominantBaseline="central"
      fontSize={26}
      fontWeight={700}
      fill={ACCENT}
    >
      Hasura events
    </text>
  </g>
);

/** A grey connector poly-line terminated by an orange arrow head. */
export const AccentArrow = ({
  points,
  headAt,
  headDir,
}: {
  /** Poly-line points, e.g. "10,20 40,20". */
  points: string;
  /** Tip of the arrow head. */
  headAt: [number, number];
  /** Direction the arrow points. */
  headDir: 'right' | 'left' | 'up' | 'down';
}) => {
  const [hx, hy] = headAt;
  const s = 7;
  const head =
    headDir === 'right'
      ? `${hx},${hy} ${hx - s},${hy - s} ${hx - s},${hy + s}`
      : headDir === 'left'
        ? `${hx},${hy} ${hx + s},${hy - s} ${hx + s},${hy + s}`
        : headDir === 'up'
          ? `${hx},${hy} ${hx - s},${hy + s} ${hx + s},${hy + s}`
          : `${hx},${hy} ${hx - s},${hy - s} ${hx + s},${hy - s}`;
  return (
    <g>
      <polyline
        points={points}
        fill="none"
        stroke={ACCENT}
        strokeWidth={2.5}
        strokeLinecap="round"
        strokeLinejoin="round"
      />
      <polygon points={head} fill={ACCENT} />
    </g>
  );
};

/**
 * The "Hasura events" engine fanning out to its three delivery targets
 * (Serverless function / Microservice API / Event queue). Shared by the Event,
 * Cron and Scheduled diagrams, which differ only in the left-hand trigger and
 * therefore in the horizontal offsets of the engine + target column.
 */
export const EngineWithOutputs = ({
  boxX,
  outX,
  boxWidth = 314,
  outWidth = 314,
}: {
  /** Left edge of the "Hasura events" box. */
  boxX: number;
  /** Left edge of the right-hand target column. */
  outX: number;
  boxWidth?: number;
  outWidth?: number;
}) => {
  const engineRight = boxX + boxWidth;
  return (
    <g>
      <HasuraEventsBox x={boxX} y={128} width={boxWidth} height={110} />

      <AccentArrow
        points={`${engineRight},183 ${outX},43`}
        headAt={[outX, 43]}
        headDir="right"
      />
      <AccentArrow
        points={`${engineRight},183 ${outX},183`}
        headAt={[outX, 183]}
        headDir="right"
      />
      <AccentArrow
        points={`${engineRight},183 ${outX},323`}
        headAt={[outX, 323]}
        headDir="right"
      />

      <OutputBox
        x={outX + 2}
        y={2}
        width={outWidth}
        height={82}
        label="Serverless function"
      />
      <OutputBox
        x={outX + 2}
        y={142}
        width={outWidth}
        height={82}
        label="Microservice API"
      />
      <OutputBox
        x={outX + 2}
        y={282}
        width={outWidth}
        height={82}
        label="Event queue"
      />
    </g>
  );
};

/** Props accepted by every top-level illustration component. */
export type IllustrationProps = SVGProps<SVGSVGElement> & {
  /** Accessible name; falls back to a sensible per-diagram default. */
  title?: string;
};
