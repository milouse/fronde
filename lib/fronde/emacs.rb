# frozen_string_literal: true

require_relative 'config'

module Fronde
  # Wraps Gnu/Emacs calls
  class Emacs
    def initialize(file_path: nil, verbose: false)
      @file = file_path
      @verbose = verbose
    end

    def publish
      command = emacs_command(
        '-l ./var/lib/org-config.el', '--eval \'(org-publish "website")\''
      )
      if @verbose
        warn command
        return system(command, exception: true)
      end
      system command, out: '/dev/null', err: '/dev/null', exception: true
    end

    private

    def emacs_command(*arguments)
      default_emacs = Fronde::CONFIG.get('emacs')
      emacs_cmd = [
        default_emacs || 'emacs -Q --batch -nw',
        '--eval \'(setq enable-dir-local-variables nil)\''
      ]
      emacs_cmd << '--eval \'(setq inhibit-message t)\'' unless @verbose
      emacs_cmd.concat(arguments)
      emacs_cmd.join(' ')
    end
  end
end
