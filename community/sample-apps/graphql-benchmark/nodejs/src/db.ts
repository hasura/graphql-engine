import postgres from 'postgres'

const sql = postgres(process.env["PG_DATABASE_URL"] || 'postgres://postgres:postgrespassword@postgres:5432/postgres', { ssl: 'require' });

export default sql;
