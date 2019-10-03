
| Opertors | Examples |
| ------ | ------ |
| _eq (**equal to** operator) |query{<br>author(<br>where: {id: {_eq: 3}}<br>){<br>id<br>name<br>}<br>}<br>{<br>"data": {<br>"author": [<br>{<br>"id": 3,<br>"name": "Sidney"<br>}<br>]<br>}<br>}|
| Content Cell  | Content Cell  |
