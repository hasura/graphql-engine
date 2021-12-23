require 'json'

def lambda_handler(event:, context:)
  body = JSON.parse(event['body'])
  table_name = body['table']['name']
  op = body['event']['op']
  data = body['event']['data']

  response = case op
             when 'INSERT' then insert_message(data, table_name)
             when 'UPDATE' then update_message(data, table_name)
             when 'DELETE' then delete_message(data, table_name)
             end

  { statusCode: 200, body: response }
rescue
  { statusCode: 400, body: 'Cannot parse hasura event' }
end

def insert_message(data, table_name)
  "New record #{new(data)} inserted into table #{table_name}"
end

def update_message(data, table_name)
  "Record changed from #{old(data)} to #{new(data)} in table #{table_name}"
end

def delete_message(data, table_name)
  "Record #{old(data)} was deleted from table #{table_name}"
end

def new(data)
  JSON.generate(data['new'])
end

def old(data)
  JSON.generate(data['old'])
end
