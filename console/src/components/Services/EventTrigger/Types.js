const Integers = ['serial', 'integer', 'bigserial', 'smallint', 'bigint'];
const Reals = ['float4', 'float8', 'numeric'];
const Numerics = [...Integers, ...Reals];

export { Numerics, Integers, Reals };
