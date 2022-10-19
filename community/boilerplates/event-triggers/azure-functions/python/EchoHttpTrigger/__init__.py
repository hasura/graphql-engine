import logging

import azure.functions as func

from enum import Enum


class Note :
    id = None
    note = None
    def __init__(self, id, note):
        self.id = id
        self.note=note
    def  getId(self) :
        return self.id
    def setId(self, id) :
        self.id = id
    def  getNote(self) :
        return self.note
    def setNote(self, note) :
        self.note = note

        
        
class Notebook :
    class Operation(Enum) :
            INSERT = 0
            UPDATE = 1
            DELETE = 2
    def  handleOperation(self, operation,  newNote,  oldNote) :
        if (operation==Notebook.Operation.INSERT):
            return "New note " + str(newNote.getId()) + " inserted, with data: " + newNote.getNote()
        elif(operation==Notebook.Operation.UPDATE):
            return "Note " + str(newNote.getId()) + " updated, with data: " + newNote.getNote()
        elif(operation==Notebook.Operation.DELETE):
            return "Note " + str(oldNote.getId()) + " deleted, with data: " + oldNote.getNote()
        else:
            raise Exception("operation: " + str(operation))        
                
                
def __inti__(self):
    self.notebook = Notebook()

def handleRequest(req_body):
    table= req_body.get('table')
    if (table != NULL and table == 'notes'):
        operation = Notebook.Operation.valueOf(req_body.get("event.op"));
        newNote = Note(req_body.get('event.data.new.id'), req_body.get('event.data.new.note'))
        oldNote = Note(req_body.get('event.data.old.id'), req_body.get('event.data.old.note'))
        return self.notebook.handleOperation(operation, newNote, oldNote)
    else:
        return "Table not supported " + table  


def main(req: func.HttpRequest) -> func.HttpResponse:
    logging.info('Python HTTP trigger function processed a request.')

    # name = req.params.get('name')
    try:
        req_body = req.get_json()
    except ValueError:
        pass
        # name = req_body.get('name')

    response = handleRequest(req_body);
    
    return func.HttpResponse(response, status_code=200)
    # if name:
    #     return func.HttpResponse(f"Hello, {name}. This HTTP triggered function executed successfully.")
    # else:
    #     return func.HttpResponse(
    #          "This HTTP triggered function executed successfully. Pass a name in the query string or in the request body for a personalized response.",
    #          status_code=200
    #     )
