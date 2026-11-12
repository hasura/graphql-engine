export function roundNumber(input, precision = 0) {
  const power = 10 ** precision;

  return Math.round(input * power) / power;
}
