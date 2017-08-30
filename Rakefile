# frozen_string_literal: true

Dir.glob('lib/tasks/*.rake').each { |r| import r }

require 'rubocop/rake_task'
RuboCop::RakeTask.new

# task :default => [:rubocop, :spec, :cucumber]

task :default => [:rubocop]
