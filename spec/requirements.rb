# frozen_string_literal: true

require 'fileutils'
require 'simplecov'

require 'r18n-core'
R18n.default_places = File.expand_path('../locales', __dir__)
R18n.set 'en'
R18n::Filters.on(:named_variables)

SimpleCov.start do
  # Activate new covering precision
  enable_coverage :branch

  # Remove dev only tools from coverage check
  add_filter ['lib/tasks/sync.rake', 'lib/tasks/doc.rake']
  # Sort coverage results into usefull groups
  add_group 'Core libs', 'lib/neruda'
  add_group 'Rake tasks', 'lib/tasks'
  add_group 'Specs', 'spec'
end

# The following requires other components automatically
require 'neruda/org_file'
require 'neruda/utils'

ENV['LANG'] = 'en'
ENV['USER'] = 'alice'

# rubocop:disable Metrics/MethodLength
def init_testing_website
  FileUtils.mkdir_p 'tmp/website_testing'
  Dir.chdir 'tmp/website_testing'
  rakefile = <<~RAKE
    # frozen_string_literal: true

    require 'r18n-core'
    R18n.default_places = '../../locales'
    R18n.set 'en'
    R18n::Filters.on(:named_variables)

    $LOAD_PATH.unshift('../../lib')

    Dir.glob('../../lib/tasks/*.rake').each { |r| import r }

    task default: 'site:build'
  RAKE
  IO.write('Rakefile', rakefile)
  config = <<~CONF
    ---
    author: Tata
    title: This is a website about test
    org-html:
      html-head-include-default-style: nil
      html-head: |
        <link rel="stylesheet" type="text/css" media="screen" href="style.css"/>
      html-postamble: '<footer>Published by Neruda.</footer>'
    sources:
    - name: org
      path: src
      target: .
      recursive: false
      exclude: tata\\.org
    - path: src/news
      is_blog: true
    - path: 'titi/test'
      recursive: nil
      exclude: ugly\\.org
    - 'titi/tutu/tata'
    templates:
    - type: before
      selector: title
      content: |
        <meta property="neruda-test" content="test"/>
    preview:
      routes:
        /test: public_html/index.html
  CONF
  IO.write('config.yml', config)
end
# rubocop:enable Metrics/MethodLength

def copy_org_tarball_to_fake_tmp
  tarball = File.expand_path(
    "../tmp/org-#{Neruda::Config.org_last_version}.tar.gz",
    __dir__
  )
  FileUtils.mkdir 'tmp'
  FileUtils.cp tarball, 'tmp'
end

def rake(verbose: false)
  rake = Rake.application
  rake.raw_load_rakefile
  Rake.verbose(verbose)
  rake.options.build_all = true
  rake.tasks.each(&:reenable)
  rake
end

RSpec.configure do |config|
  config.before(:suite) do
    Neruda::Utils.download_org
  end

  config.after(:suite) do
    FileUtils.rm_r 'tmp', force: true
  end
end
