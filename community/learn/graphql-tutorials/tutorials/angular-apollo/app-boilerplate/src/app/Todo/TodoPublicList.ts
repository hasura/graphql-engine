import { Component, Input } from '@angular/core'; 

@Component({  
    selector: 'TodoPublicList',  
    templateUrl: './TodoPublicList.template.html',  
  }) 

export class TodoPublicList {
          olderTodosAvailable= true;
          newTodosCount= 1;
          todos= [
            {
              id: "1",
              title: "This is public todo 1",
              user: {
                name: "someUser1"
              }
            },
            {
              id: "2",
              title: "This is public todo 2",
              is_completed: false,
              is_public: true,
              user: {
                name: "someUser2"
              }
            },
            {
              id: "3",
              title: "This is public todo 3",
              user: {
                name: "someUser3"
              }
            },
            {
              id: "4",
              title: "This is public todo 4",
              user: {
                name: "someUser4"
              }
            }
          ]
    
      loadNew() {}
    
      loadOlder() {}
    
}
