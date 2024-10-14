# frozen_string_literal: true

require_relative 'config'

module Fronde
  # Wraps Gnu/Emacs calls
  class Emacs
    def initialize(verbose: false)
      @verbose = verbose
      @command = nil
    end

    def publish(project: 'website', force: false)
      org_action = ['org-publish', %("#{project}")]
      org_action << 't' if force
      build_command "(#{org_action.join(' ')})"
      run_command
    end

    private

    def run_command
      cmd = @command.join(' ')
      if @verbose
        warn cmd
        return system(cmd, exception: true)
      end
      system cmd, out: File::NULL, err: File::NULL, exception: true
    end

    def build_command(org_action)
      default_emacs = Fronde::CONFIG.get('emacs')
      @command = [default_emacs || 'emacs -Q --batch -nw']
      @command << '--eval \'(setq inhibit-message t)\'' unless @verbose
      @command += [
        '--eval \'(setq enable-dir-local-variables nil)\'',
        '-l ./var/lib/org-config.el',
        "--eval '#{org_action}'"
      ]
    end
  end
end
