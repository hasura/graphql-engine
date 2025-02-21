-- Create a basic table
CREATE TABLE IF NOT EXISTS users
(
    id
    SERIAL
    PRIMARY
    KEY,
    email
    VARCHAR
(
    255
) NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    );

-- Create index concurrently (must be run separately, not in transaction)
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_users_email
    ON users (email);