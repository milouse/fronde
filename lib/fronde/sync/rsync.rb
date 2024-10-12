# frozen_string_literal: true

require_relative '../config'

module Fronde
  module Sync
    # Everything needed to push or pull data with rsync
    class Rsync
      def initialize(remote_path, local_path, verbose: false)
        @verbose = verbose
        @remote_path = remote_path
        @local_path = "#{local_path}/"
      end

      def pull(test: false)
        run command(test:) + [@remote_path, @local_path]
      end

      def push(test: false)
        run command(test:) + [@local_path, @remote_path]
      end

      private

      def run(cmd)
        warn cmd.join(' ') if @verbose
        # Be precise about Kernel to allow mock in rspec
        Kernel.system(*cmd)
      end

      def command(test: false)
        rsync_command = Fronde::CONFIG.get('rsync')
        return rsync_command unless rsync_command.nil?

        optstring = []
        optstring << 'n' if test
        if @verbose
          optstring << 'v'
        else
          optstring << 'q'
        end
        ['rsync', "-#{optstring.join}rlt", '--delete']
      end
    end
  end
end
