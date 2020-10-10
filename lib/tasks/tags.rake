# frozen_string_literal: true

require 'neruda/index'

namespace :tags do
  desc 'List all tags by name'
  task :name do
    index = Neruda::Index.new
    next if index.empty?
    puts index.sort_by(:name).join("\n")
  end

  desc 'List all tags by weight'
  task :weight do
    index = Neruda::Index.new
    next if index.empty?
    puts index.sort_by(:weight).join("\n")
  end
end
