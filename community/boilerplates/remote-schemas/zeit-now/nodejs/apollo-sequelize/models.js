const Sequelize = require("sequelize");

const POSTGRES_CONNECTION_STRING = process.env.POSTGRES_CONNECTION_STRING || "postgres://postgres:password@localhost:6432/postgres";

const sequelize = new Sequelize(
    POSTGRES_CONNECTION_STRING, {}
);

const User = sequelize.define(
    'user',
    {
        id: { type: Sequelize.INTEGER, autoIncrement: true, primaryKey: true },
        name: Sequelize.TEXT,
        balance: Sequelize.INTEGER
    },
    {
        timestamps: false
    }
);

const MinAmount = sequelize.define(
    'min_amount',
    {
        amount: Sequelize.INTEGER
    },
    {
        freezeTableName: true,
        timestamps: false
    }
);

MinAmount.removeAttribute('id');

exports.sequelize = sequelize;
exports.User = User;
exports.MinAmount = MinAmount;

