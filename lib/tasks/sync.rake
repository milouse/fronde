# frozen_string_literal: true

require 'fronde/config'
require 'fronde/utils'

module Fronde
  class SyncError < Error; end
end

def rsync_command(test = nil)
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

def pull_or_push(direction, standard, test)
  unless [:pull, :push].include? direction
    raise Fronde::SyncError, 'Not a valid direction'
  end
  remote_path = Fronde::Config.get("#{standard}_remote")
  raise Fronde::SyncError, "No #{standard} remote path set" if remote_path.nil?
  public_folder = Fronde::Config.get("#{standard}_public_folder")
  # Default is to push
  cmd = ["#{public_folder}/", remote_path]
  cmd.reverse! if direction == :pull
  rsync = rsync_command(test)
  Thread.new do
    sh "#{rsync} #{cmd.join(' ')}"
  end
end

namespace :sync do
  desc 'Push changes to server'
  task :push, :test? do |_, args|
    ['html', 'gemini'].each do |standard|
      publish_thread = pull_or_push(:push, standard, args[:test?])
      if verbose
        publish_thread.join
      else
        Fronde::Utils.throbber(
          publish_thread, format('Publishing %<fmt>s:', fmt: standard)
        )
      end
    end
  rescue Fronde::SyncError => e
    warn e
    next
  end

  desc 'Pull changes from server'
  task :pull, :test? do |_, args|
    ['html', 'gemini'].each do |standard|
      pull_thread = pull_or_push(:pull, standard, args[:test?])
      if verbose
        pull_thread.join
      else
        Fronde::Utils.throbber(
          pull_thread, format('Pulling %<fmt>s:', fmt: standard)
        )
      end
    end
  rescue Fronde::SyncError => e
    warn e
    next
  end
end
