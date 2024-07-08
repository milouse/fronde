# frozen_string_literal: true

require_relative '../fronde/index'

namespace :tags do
  desc 'List all tags by name'
  task :name do
    Fronde::Index.all_blog_index do |index|
      next if index.empty?

      puts index.sort_by(:name).join("\n")
    end
  end

  desc 'List all tags by weight'
  task :weight do
    Fronde::Index.all_blog_index do |index|
      next if index.empty?

      puts index.sort_by(:weight).join("\n")
    end
  end
end
