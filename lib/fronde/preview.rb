# frozen_string_literal: true

require 'webrick'
require 'fronde/config'

module Fronde # rubocop:disable Style/Documentation
  # A tiny preview server, which main goal is to replace references to
  # the target domain by localhost.
  class PreviewServlet < WEBrick::HTTPServlet::AbstractServlet
    include WEBrick::HTTPUtils

    def do_GET(request, response) # rubocop:disable Naming/MethodName
      file = local_path(request.path)
      response.body = parse_body(file, "http://#{request.host}:#{request.port}")
      response.status = 200
      response.content_type = mime_type(file, DefaultMimeTypes)
    end

    private

    def local_path(requested_path)
      routes = Fronde::Config.settings.dig('preview', 'routes') || {}
      return routes[requested_path] if routes.has_key? requested_path
      local_path = Fronde::Config.settings['public_folder'] + requested_path
      if File.directory? local_path
        local_path = format(
          '%<path>s/index.html', path: local_path.delete_suffix('/')
        )
      end
      return local_path if File.exist? local_path
      raise WEBrick::HTTPStatus::NotFound, 'Not found.'
    end

    def parse_body(local_path, local_host)
      body = IO.read local_path
      return body unless local_path.match?(/\.(?:ht|x)ml\z/)
      domain = Fronde::Config.settings['domain']
      return body if domain == ''
      body.gsub(/"file:\/\//, format('"%<host>s', host: local_host))
          .gsub(/"#{domain}/, format('"%<host>s', host: local_host))
    end
  end

  class << self
    def start_preview
      # Inspired by ruby un.rb library, which allows normally to start a
      # webrick server in one line: ruby -run -e httpd public_html -p 5000
      port = Fronde::Config.settings.dig('preview', 'server_port') || 5000
      s = WEBrick::HTTPServer.new(Port: port)
      s.mount '/', Fronde::PreviewServlet
      ['TERM', 'QUIT', 'INT'].each { |sig| trap(sig, proc { s.shutdown }) }
      s.start
    end
  end
end
