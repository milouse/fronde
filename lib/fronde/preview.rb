# frozen_string_literal: true

require 'webrick'
require_relative 'config'

module Fronde
  module Preview # rubocop:disable Style/Documentation
    # A tiny preview server, which main goal is to replace references to
    #   the target domain by localhost.
    class Servlet < WEBrick::HTTPServlet::AbstractServlet
      include WEBrick::HTTPUtils

      def do_GET(request, response) # rubocop:disable Naming/MethodName
        file = local_path(request.path)
        response.body = parse_body(file, "http://#{request.host}:#{request.port}")
        response.status = 200
        response.content_type = mime_type(file, DefaultMimeTypes)
      end

      private

      def local_path(requested_path)
        routes = Fronde::CONFIG.get(%w[preview routes], {})
        return routes[requested_path] if routes.has_key? requested_path

        local_path = Fronde::CONFIG.get('html_public_folder') + requested_path
        if File.directory? local_path
          local_path = format(
            '%<path>s/index.html', path: local_path.delete_suffix('/')
          )
        end
        return local_path if File.exist? local_path

        raise WEBrick::HTTPStatus::NotFound, 'Not found.'
      end

      def parse_body(local_path, local_host)
        body = File.read local_path
        return body unless local_path.match?(/\.(?:ht|x)ml\z/)

        domain = Fronde::CONFIG.get('domain')
        return body if domain == ''

        host_repl = %("#{local_host})
        body.gsub('"file://', host_repl).gsub(%("#{domain}), host_repl)
      end
    end

    def self.start
      # Inspired by ruby un.rb library, which allows normally to start a
      # webrick server in one line: ruby -run -e httpd public_html -p 5000
      port = Fronde::CONFIG.get(%w[preview server_port], 5000)
      s = WEBrick::HTTPServer.new(Port: port)
      s.mount '/', Servlet
      %w[TERM QUIT INT].each { |sig| trap(sig, proc { s.shutdown }) }
      s.start
    end
  end
end
