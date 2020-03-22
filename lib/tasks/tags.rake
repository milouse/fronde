# frozen_string_literal: true

require 'neruda/index'

namespace :tags do
  desc 'List all tags by name'
  task :name do
    blog_path = Neruda::Config.settings['blog_path']
    next if blog_path.nil?
    next unless Dir.exist?("src/#{blog_path}")
    index = Neruda::Index.new
    puts index.sort_by(:name).join("\n")
  end

  desc 'List all tags by weight'
  task :weight do
    blog_path = Neruda::Config.settings['blog_path']
    next if blog_path.nil?
    next unless Dir.exist?("src/#{blog_path}")
    index = Neruda::Index.new
    puts index.sort_by(:weight).join("\n")
  end
end
