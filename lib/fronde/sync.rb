# frozen_string_literal: true

require_relative 'config'
require_relative 'sync/rsync'
require_relative 'sync/neocities'

module Fronde
  # Entrypoint for synchronization with remote public server
  module Sync
    class Error < ::StandardError; end

    ALLOWED_SYNCER = %w[rsync neocities].freeze

    def self.extract_method_and_remote(type)
      remote_path = Fronde::CONFIG.get("#{type}_remote")
      raise Error, "No #{type} remote path set" if remote_path.nil?

      method, remote = remote_path.split(':', 2)
      return [method, remote] if ALLOWED_SYNCER.include?(method)

      ['rsync', remote_path]
    end

    def self.pull_or_push(direction, type, test: false, verbose: false)
      method, remote_path = extract_method_and_remote type
      public_folder = Fronde::CONFIG.get("#{type}_public_folder")
      klass = Kernel.const_get("::Fronde::Sync::#{method.capitalize}")
      syncer = klass.new(remote_path, public_folder, verbose: verbose)
      Thread.new { syncer.send(direction, test: test) }
    end
  end
end
