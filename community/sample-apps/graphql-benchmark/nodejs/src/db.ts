import postgres from 'postgres'

const sql = postgres(process.env["PG_DATABASE_URL"] || 'postgres://postgres:postgrespassword@postgres:5432/postgres', { ssl: 'require' });
// const sql = postgres('postgres://postgres:postgrespassword@postgres:5432/postgres');
// const sql = postgres('postgres://postgres:postgrespassword@localhost:5432/postgres');

export default sql;
