# frozen_string_literal: true

$LOAD_PATH.unshift('./lib')

# The following requires other components automatically
require 'neruda/org_file'

require 'r18n-core'
R18n.set('en', File.expand_path('../locales', __dir__))
