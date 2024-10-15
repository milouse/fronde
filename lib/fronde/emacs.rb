# frozen_string_literal: true

require_relative 'config'

module Fronde
  # Wraps Gnu/Emacs calls
  class Emacs
    def initialize(verbose: false)
      @verbose = verbose
      @command = nil
    end

    def publish(project = 'website', force: false)
      if force
        build_command %[(org-publish "#{project}" t)]
      else
        build_command %[(org-publish "#{project}")]
      end
      run_command
    end

    def publish_file(file_path, force: false)
      if force
        build_command '(org-publish-current-file t)'
      else
        build_command '(org-publish-current-file)'
      end
      @command.insert(-2, %(--visit "#{file_path}"))
      run_command
    end

    private

    def run_command
      cmd = @command.join(' ')
      if @verbose
        puts cmd
        return system(cmd, err: $stdout, exception: true)
      end
      system cmd, out: File::NULL, err: File::NULL, exception: true
    end

    def build_command(org_action)
      default_emacs = Fronde::CONFIG.get('emacs')
      @command = [
        default_emacs || 'emacs -Q --batch -nw',
        '--eval \'(setq inhibit-message t)\'',
        '-l ./var/lib/org-config.el',
        "--eval '#{org_action}'"
      ]
      @command.delete_at(1) if @verbose
    end
  end
end
