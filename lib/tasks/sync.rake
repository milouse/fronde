# frozen_string_literal: true

require 'neruda/config'

rsync_command = Neruda::Config.settings['rsync'] || 'rsync -nvrlpD --delete'
remote_path = Neruda::Config.settings['remote']
public_folder = Neruda::Config.settings['public_folder'] || 'public_html'

namespace :sync do
  desc 'Push change to server'
  task :push do
    if remote_path.nil?
      warn 'No remote path set'
      next
    end
    sh [rsync_command, "#{public_folder}/", remote_path].join(' ')
  end

  desc 'Pull change from server'
  task :pull do
    if remote_path.nil?
      warn 'No remote path set'
      next
    end
    sh [rsync_command, remote_path, "#{public_folder}/"].join(' ')
  end
end
