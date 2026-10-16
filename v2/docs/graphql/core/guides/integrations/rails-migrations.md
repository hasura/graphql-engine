# Using Rails Migrations with Hasura

This guide explains how to use **Ruby on Rails migrations** to manage your database schema while using **Hasura GraphQL Engine**.

## Prerequisites

- A running Hasura instance connected to your PostgreSQL database  
- A working Ruby on Rails project  
- Rails migrations enabled in your app (default in most Rails apps)

---

## Step 1: Create a Rails migration

You can create a new migration as usual with Rails:

```bash
rails generate migration AddUsersTable
