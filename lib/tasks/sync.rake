# frozen_string_literal: true

require 'neruda/config'

def rsync_command(verbose, test = nil)
  rsync_command = Neruda::Config.settings['rsync']
  return rsync_command unless rsync_command.nil?
  optstring = []
  optstring << 'n' if test
  if verbose
    optstring << 'v'
  else
    optstring << 'q'
  end
  "rsync -#{optstring.join}rlpD --delete"
end

namespace :sync do
  desc 'Push change to server'
  task :push, :test? do |_, args|
    remote_path = Neruda::Config.settings['remote']
    if remote_path.nil?
      warn 'No remote path set'
      next
    end
    public_folder = Neruda::Config.settings['public_folder']
    sh [rsync_command(Rake::FileUtilsExt.verbose_flag, args[:test?]),
        "#{public_folder}/", remote_path].join(' ')
  end
end
