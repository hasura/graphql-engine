require 'json'
require 'net/http'

def lambda_handler(event:, context:)
  access_key = ENV['ACCESS_KEY']
  hge_endpoint = ENV['HGE_ENDPOINT']
  query = <<~QUERY
    mutation updateNoteRevision ($noteId: Int!, $data: String!) {
      insert_note_revision (objects: [
        {
          note_id: $noteId,
          note: $data
        }
      ]) {
        affected_rows
      }
    }
  QUERY

  note_id = JSON.parse(event['body'])['event']['data']['old']['id']
  data = JSON.parse(event['body'])['event']['data']['old']['note']

  uri = URI.parse(hge_endpoint + '/v1/graphql')
  use_ssl = (uri.scheme == 'https')

  res = Net::HTTP.start(uri.host, uri.port, use_ssl: use_ssl) do |http|
    req = Net::HTTP::Post.new(uri)
    req['Content-Type'] = 'application/json'
    req['x-hasura-access-key'] = access_key
    req.body = JSON.generate(
      query: query,
      variables: { noteId: note_id, data: data }
    )
    http.request(req)
  end

  if errors = JSON.parse(res.body)['errors']
    return { statusCode: 400, body: JSON.generate(errors) }
  end

  { statusCode: 200, body: 'success' }
rescue
  { statusCode: 400, body: 'cannot parse hasura event' }
end
