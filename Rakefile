# frozen_string_literal: true

require 'yard'

$LOAD_PATH.unshift('./lib')
import './lib/tasks/org.rake'

YARD::Rake::YardocTask.new do |t|
  t.options = ['-m', 'org', '--no-progress']
end
