# frozen_string_literal: true

require 'fileutils'
require 'simplecov'

require 'r18n-core'
R18n.set('en', File.expand_path('../locales', __dir__))

$LOAD_PATH.unshift('./lib')

SimpleCov.start do
  add_group 'Core libs', 'lib/neruda'
  add_group 'Rake tasks', 'lib/tasks'
  add_group 'Specs', 'spec'
end

# The following requires other components automatically
require 'neruda/org_file'

def init_testing_website
  FileUtils.mkdir_p 'spec/data/website_testing'
  Dir.chdir 'spec/data/website_testing'
  rakefile = <<~RAKE
    # frozen_string_literal: true

    Dir.glob('../../../lib/tasks/*.rake').each { |r| import r }

    task default: 'site:build'
  RAKE
  IO.write('Rakefile', rakefile)
  config = <<~CONF
    ---
    author: Tata
    title: This is a website about test
    exclude_pattern: tata\\.org
    org-html:
      html-head: |
        <link rel="stylesheet" type="text/css" media="screen" href="style.css"/>
    external_sources:
    - path: 'titi/test'
      recursive: nil
      exclude: ugly\\.org
    - 'titi/tutu/tata'
    templates:
    - type: before
      selector: title
      content: |
        <meta property="neruda-test" content="test"/>
  CONF
  IO.write('config.yml', config)
end

def init_rake_and_install_org
  # When run with all other specs, config may have been already loaded
  Neruda::Config.send(:load_settings)
  rake = Rake.application
  rake.raw_load_rakefile
  Rake.verbose(false)
  rake.options.build_all = true
  rake.tasks.each(&:reenable)
  rake.invoke_task('org:install')
  rake
end
