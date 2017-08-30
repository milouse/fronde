# frozen_string_literal: true

require 'fileutils'

namespace :test do
  desc 'Clean unwanted files'
  task :cleanup do
    ['config', 'private', 'public', 'tmp', 'views'].each do |devdir|
      FileUtils.rm_r(devdir) if Dir.exist? devdir
    end
    File.unlink('config.ru') if File.exist? 'config.ru'
  end
end
