# frozen_string_literal: true

require 'fronde/config'
require 'fronde/utils'

def rsync_command(verbose, test = nil)
  rsync_command = Fronde::Config.get('rsync')
  return rsync_command unless rsync_command.nil?
  optstring = []
  optstring << 'n' if test
  if verbose
    optstring << 'v'
  else
    optstring << 'q'
  end
  "rsync -#{optstring.join}rlt --delete"
end

namespace :sync do
  desc 'Push change to server'
  task :push, :test? do |_, args|
    remote_path = Fronde::Config.get('remote')
    if remote_path.nil?
      warn 'No remote path set'
      next
    end
    public_folder = Fronde::Config.get('public_folder')
    publish_thread = Thread.new do
      sh [rsync_command(Rake::FileUtilsExt.verbose_flag, args[:test?]),
          "#{public_folder}/", remote_path].join(' ')
    end
    Fronde::Utils.throbber(publish_thread, 'Publishing:')
  end
end
