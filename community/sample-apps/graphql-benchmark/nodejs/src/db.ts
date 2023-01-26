import postgres from 'postgres'

const sql = postgres('postgres://postgres:postgrespassword@localhost:5432/postgres');

export default sql;
