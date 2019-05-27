# frozen_string_literal: true

require 'neruda/config'
require 'r18n-core'

R18n.set(Config.settings['lang'] || 'en')

Dir.glob('lib/tasks/*.rake').each { |r| import r }

task default: 'site:build'
