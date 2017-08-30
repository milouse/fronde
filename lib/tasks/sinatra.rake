# frozen_string_literal: true

require 'rainbow'

namespace :sinatra do
  desc 'Stop the underlaying sinatra application'
  task :stop do
    unless File.exist? 'tmp/pids/neruda.pid'
      STDERR.puts Rainbow('No pid file found').red
      exit 1
    end
    pid = IO.read('tmp/pids/neruda.pid').strip.to_i
    Process.kill('TERM', pid)
    File.unlink 'tmp/pids/neruda.pid'
    puts Rainbow('Done').green
  end

  desc 'Start the underlaying sinatra application'
  task :start do
    loc_env = ENV['APP_ENV'] || 'development'
    cmd = ['rackup', "-E #{loc_env}", '-P', 'tmp/pids/neruda.pid']
    cmd << '-D' if loc_env == 'production'
    begin
      if ENV['APP_ENV'].nil?
        system({ 'APP_ENV' => loc_env }, cmd.join(' '))
      else
        sh(*cmd)
      end
    rescue Interrupt
      puts Rainbow('Kthxbye').blue
    end
  end

  desc 'Restart local sinatra server'
  task restart: ['sinatra:stop', 'sinatra:start']
end
