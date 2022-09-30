export function roundNumber(input, precision = 0) {
  const power = 10 ** precision;

  return Math.round(input * power) / power;
}

export function formatRoundNumber(input, precision = 0) {
  return new Intl.NumberFormat('en-US', {
    minimumFractionDigits: precision,
    maximumFractionDigits: precision,
  }).format(input);
}

export const secondToMillisecs = input => input * 1000;
