# frozen_string_literal: true

require 'uri'

# Various method to ease url handling
module Neruda::Url
  def proxy_url(url)
    wanted_uri = url(url)
    return wanted_uri if Neruda::CONFIG['base_path'].nil?
    uri_data = URI.parse(wanted_uri)
    uri_data.path = Neruda::CONFIG['base_path'] + uri_data.path
    uri_data.to_s
  end
end
