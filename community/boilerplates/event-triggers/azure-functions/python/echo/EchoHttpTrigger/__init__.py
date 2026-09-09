import logging

import azure.functions as func

def main(req: func.HttpRequest) -> func.HttpResponse:
    logging.info('Python HTTP trigger function processed a request.')

    if(req.body!=NULL):    
        return func.HttpResponse(req.body)
    else:
        return func.HttpResponse(status_code=400, body="No request body")