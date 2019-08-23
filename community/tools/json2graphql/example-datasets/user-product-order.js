const db = {
  user: [
    { id: 1, name: "Jon Doe", date_of_birth: new Date('Jan 8, 1992 00:00:00')},
    { id: 2, name: "Jon Doe", date_of_birth: new Date('May 8, 1982 00:00:00')},
    { id: 3, name: "Jon Doe", date_of_birth: new Date('Dec 8, 1972 00:00:00')}
  ],
  product: [
    { id: 1, name: "Samsung TV", price: "4000", available: 6},
    { id: 2, name: "Apple TV", price: "8000", available: 2},
    { id: 3, name: "Playstation", price: "800", available: 20}
  ],
  order: [
    { id: 1, user_id: 1, product_id: 2, placed_at: new Date('Jan 22, 2019 00:12:10')},
    { id: 2, user_id: 3, product_id: 1, placed_at: new Date('Jan 26, 2019 00:12:10')}
  ]
}

module.exports = db;