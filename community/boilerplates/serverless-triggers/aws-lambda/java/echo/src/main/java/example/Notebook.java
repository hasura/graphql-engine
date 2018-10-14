package example;

class Notebook {

    enum Operation {
        INSERT, UPDATE, DELETE;
    }

    String handleOperation(Operation operation, Note newNote, Note oldNote) {
        switch (operation) {
            case INSERT:
                return "New note " + newNote.getId() + " inserted, with data: " + newNote.getNote();
            case UPDATE:
                return "Note " + newNote.getId() + " updated, with data: " + newNote.getNote();
            case DELETE:
                return "Note " + oldNote.getId() + " deleted, with data: " + oldNote.getNote();
            default:
                throw new UnsupportedOperationException("operation: " + operation);
        }
    }

}
