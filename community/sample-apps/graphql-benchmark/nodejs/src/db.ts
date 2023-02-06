import postgres from 'postgres'

const sql = postgres('postgres://postgres:postgrespassword@postgres:5432/postgres');

export default sql;
