# frozen_string_literal: true

require 'json'
require 'yaml'
require 'net/http'

module Fronde
  # A wrapper around Mastodon API to ease publication of new statuses
  class MastodonAuth
    COMMON_PARAMS = [
      ['redirect_uri', 'urn:ietf:wg:oauth:2.0:oob'],
      ['scope', 'write:statuses']
    ].freeze

    def initialize(instance_domain)
      @instance_domain = instance_domain
      @connection = nil
      @credentials = {}
      read_credentials
    end

    def bootstrap
      init_connection
      register_new_app
      create_access_code
      authorize_user
      @connection.finish
      write_credentials
    end

    def auth
      init_connection
      authorize_user
      @connection.finish
      write_credentials
    end

    def post(status)
      init_connection
      request = build_request('api/v1/statuses', [['status', status]])
      oauth_token = @credentials['mastodon_oauth_token']
      request.headers['Authorization'] = "Bearer #{oauth_token}"
      status_data = JSON.parse(@connection.request(request).body)
      @connection.finish
      status_data['id']
    end

    private

    def read_credentials
      return unless File.exist?('.credentials')

      @credentials = YAML.load_file('.credentials')
    end

    def write_credentials
      File.write('.credentials', @credentials.to_yaml)
    end

    def init_connection
      @connection = Net::HTTP.new(@instance_domain, 443)
      @connection.use_ssl = true
      @connection.start
    end

    def build_request(endpoint, data)
      request = Net::HTTP::Post.new(
        URI("https://#{@instance_domain}/#{endpoint}")
      )
      request.set_form data, 'multipart/form-data'
      request
    end

    def register_new_app
      params = COMMON_PARAMS.map { ["#{_1[0]}s", _1[1]] } +
               [['client_name', 'Fronde test'],
                ['website', 'https://etienne.depar.is/fronde']]
      app_request = build_request 'api/v1/apps', params
      app_data = JSON.parse(@connection.request(app_request).body)
      @credentials['mastodon_client_id'] = app_data['client_id']
      @credentials['mastodon_client_secret'] = app_data['client_secret']
    end

    def create_access_code
      params = COMMON_PARAMS.map { _1.join('=') } +
               ["client_id=#{@credentials['client_id']}",
                'response_type=code']
      spawn 'gio', 'open',
            "https://#{@instance_domain}/oauth/authorize?#{params.join('&')}"
      print('Access code? ')
      @credentials['mastodon_access_code'] = $stdin.gets.strip
    end

    def authorize_user
      params = COMMON_PARAMS +
               [['client_id', @credentials['client_id']],
                ['client_secret', @credentials['client_secret']],
                ['code', @credentials['access_code']],
                %w[grant_type authorization_code]]
      auth_request = build_request 'oauth/token', params
      auth_data = JSON.parse(@connection.request(auth_request).body)
      @credentials['mastodon_oauth_token'] = auth_data['access_token']
    end
  end
end
