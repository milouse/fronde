# frozen_string_literal: true

require 'fronde/config'
require 'fronde/utils'

module Fronde
  class SyncError < Error; end
end

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

def pull_or_push(direction, label, test)
  unless [:pull, :push].include? direction
    raise Fronde::SyncError, 'Not a valid direction'
  end
  remote_path = Fronde::Config.get('remote')
  raise Fronde::SyncError, 'No remote path set' if remote_path.nil?
  public_folder = Fronde::Config.get('public_folder')
  # Default is to push
  cmd = ["#{public_folder}/", remote_path]
  cmd.reverse! if direction == :pull
  rsync = rsync_command(Rake::FileUtilsExt.verbose_flag, test)
  publish_thread = Thread.new do
    sh "#{rsync} #{cmd.join(' ')}"
  end
  Fronde::Utils.throbber(publish_thread, label)
end

namespace :sync do
  desc 'Push changes to server'
  task :push, :test? do |_, args|
    pull_or_push(:push, 'Publishing:', args[:test?])
  rescue Fronde::SyncError => e
    warn e
    next
  end

  desc 'Pull changes from server'
  task :pull, :test? do |_, args|
    pull_or_push(:pull, 'Pulling:', args[:test?])
  rescue Fronde::SyncError => e
    warn e
    next
  end
end
