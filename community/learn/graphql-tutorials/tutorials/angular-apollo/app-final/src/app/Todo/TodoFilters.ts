import { Component,  Input, Output, EventEmitter, OnChanges } from '@angular/core';


@Component({  
    selector: 'TodoFilters',  
    templateUrl: './TodoFilters.template.html',  
  }) 

export class TodoFilters implements  OnChanges{
    @Input('todos') todos: any;  
    @Input('currentFilter') currentFilter: any;  
    @Output('filterResultsFn') filterResultsFn: EventEmitter<any> = new EventEmitter();;  
    @Output('clearCompletedFn') clearCompletedFn: EventEmitter<any> = new EventEmitter();; 
    
    itemCount: any;

    ngOnChanges(){
        this.updateItemCount();
    }

    updateItemCount() {
        this.itemCount = this.todos.length;
        const activeTodos = this.todos.filter(todo => todo.is_completed !== true);

        if (this.currentFilter === 'active') {
            this.itemCount = activeTodos.length;
        } else if (this.currentFilter === 'completed') {
            this.itemCount = this.todos.length - activeTodos.length;
        }
    }

    clearCompletedFnWrapper(item: any) {
        this.clearCompletedFn.emit(item);
       
    }

    filterResultsHandler(filter) {
          this.filterResultsFn.emit({ event:event, filter: filter });
        ;
    };
}
