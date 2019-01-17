const db = {
  "user": [
    { "id": 1, "name": "John Doe", "username": "johndoe", "last_seen": new Date() },
    { "id": 2, "name": "Alice Wan", "username": "alisson", "last_seen": new Date() },
    { "id": 3, "name": "Natalie Jackson", "username": "nats", "last_seen": new Date() },
    { "id": 4, "name": "George Walsh", "username": "georgee", "last_seen": new Date() }
  ],
  "group": [
    { "id": 1, "name": "Engineering", is_active: true },
    { "id": 2, "name": "Marketting", is_active: false }
  ],
  "message": [
    { "id": 1, group_id: 1, "body": "Message 1", "sent_at": new Date(), "user_id": 1 },
    { "id": 2, group_id: 1, "body": "Message 2", "sent_at": new Date(), "user_id": 2 },
    { "id": 3, group_id: 2, "body": "Message 3", "sent_at": new Date(), "user_id": 3 },
    { "id": 4, group_id: 2, "body": "Message 4", "sent_at": new Date(), "user_id": 4 }
  ]
}

module.exports = db;