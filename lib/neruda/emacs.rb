# frozen_string_literal: true

require 'neruda/config'

module Neruda
  # Wraps Gnu/Emacs calls
  class Emacs
    def initialize(file_path: nil, verbose: false)
      @file = file_path
      @verbose = verbose
    end

    def publish
      if @file.nil?
        emacs_args = ['--eval \'(org-publish "website")\'']
      else
        emacs_args = ['-f org-publish-current-file']
      end
      call_emacs emacs_args
    end

    private

    def emacs_command(arguments = [])
      default_emacs = Neruda::Config.settings['emacs']
      emacs_cmd = [default_emacs || 'emacs -Q --batch -nw']
      emacs_cmd << '--eval \'(setq enable-dir-local-variables nil)\''
      emacs_cmd << '--eval \'(setq inhibit-message t)\'' unless @verbose
      emacs_cmd << '-l ./org-config.el'
      emacs_cmd << "--eval '(find-file \"#{@file}\")'" unless @file.nil?
      emacs_cmd.concat(arguments)
      emacs_cmd.join(' ')
    end

    def call_emacs(arguments = [])
      command = emacs_command arguments
      if @verbose
        warn command
        return system(command, exception: true)
      end
      system command, out: '/dev/null', err: '/dev/null', exception: true
    end
  end
end
