# frozen_string_literal: true

require_relative '../fronde/config'
require_relative '../fronde/sync'
require_relative '../fronde/cli/throbber'

def source_types
  Fronde::CONFIG.sources.map(&:type).uniq
end

namespace :sync do
  desc 'Push changes to server'
  task :push, :test? do |_, args|
    source_types.each do |type|
      publish_thread = Fronde::Sync.pull_or_push(
        :push, type, test: args[:test?], verbose: verbose
      )
      Fronde::CLI::Throbber.run(
        publish_thread, format('Publishing %<fmt>s:', fmt: type), verbose
      )
    rescue Fronde::Sync::Error => e
      warn e
      next
    end
  rescue RuntimeError, Interrupt
    next
  end

  desc 'Pull changes from server'
  task :pull, :test? do |_, args|
    source_types.each do |type|
      pull_thread = Fronde::Sync.pull_or_push(
        :pull, type, test: args[:test?], verbose: verbose
      )
      Fronde::CLI::Throbber.run(
        pull_thread, format('Pulling %<fmt>s:', fmt: type), verbose
      )
    rescue Fronde::Sync::Error => e
      warn e
      next
    end
  rescue RuntimeError, Interrupt
    next
  end
end
