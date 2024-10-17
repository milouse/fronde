# frozen_string_literal: true

require 'rake'

def add_some_contents
  FileUtils.mkdir_p 'src/news'
  org_content = <<~ORG
    #+title: Index file
    #+description: Nice description

    Nice content.
  ORG
  File.write('src/index.org', org_content)
  file1 = <<~ORG
    #+title: Index file
    #+keywords: toto, titi

    Lorem ipsum sic habet…
  ORG
  File.write('src/news/test1.org', file1)
end

def write_base_files
  config = <<~CONF
    ---
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
  CONF
  File.write('config.yml', config)
  Fronde::CONFIG.reset
  Fronde::CONFIG.write_org_lisp_config
  add_some_contents
  file2 = <<~ORG
    #+title: Index file
    #+keywords: toto

    Aliquam sed ex nulla
  ORG
  File.write('src/news/test2.org', file2)
end

context 'with a testing website' do
  before(:all) do # rubocop:disable RSpec/BeforeAfterAll
    init_testing_environment
    Fronde::CONFIG.reset # Take into account dir change
    init_fake_org_install
    copy_org_tarball_to_fake_tmp
    copy_org_lisp_files_to_fake_tmp
    rake.invoke_task('org:install')
  end

  after { clean_testing_environment }

  after(:all) do # rubocop:disable RSpec/BeforeAfterAll
    tear_down 'tmp/website_testing'
  end

  context 'when building org files' do
    before do
      Fronde::CONFIG.reset
      Fronde::CONFIG.write_org_lisp_config
      add_some_contents
    end

    it 'builds default project', :aggregate_failures do
      rake(verbose: false).invoke_task('site:build')
      expect(File.exist?('public_html/index.html')).to be true
      expect(File.exist?('public_html/news/test1.html')).to be true
    end

    it 'builds verbosely default project', :aggregate_failures do
      # For coverage
      rake(verbose: true).invoke_task('site:build')
      expect(File.exist?('public_html/index.html')).to be true
      expect(File.exist?('public_html/news/test1.html')).to be true
    end

    it 'builds a single file', :aggregate_failures do
      rake(verbose: false).invoke_task('site:build_file[src/index.org]')
      expect(File.exist?('public_html/index.html')).to be true
      expect(File.exist?('public_html/news/test1.html')).to be false
    end

    it 'build something for gemini projects' do
      old_conf = Fronde::CONFIG.settings.merge
      old_conf['sources'] = [
        { 'name' => 'org', 'path' => 'src', 'target' => '.',
          'type' => 'gemini' }
      ]
      Fronde::CONFIG.load_test(old_conf)
      Fronde::CONFIG.write_org_lisp_config
      rake.invoke_task('site:build')
      expect(File.exist?('public_gmi/index.gmi')).to be true
    end

    it 'extracts html body', :aggregate_failures do
      # No source file
      o = Fronde::Org::File.new(
        'test/test.org', title: 'No source', content: 'Test'
      )
      data = o.to_h
      expect(data['published_body']).to eq('')

      # Before build
      o = Fronde::Org::File.new('src/index.org')
      data = o.to_h
      expect(data['published_body']).to eq('Nice description')

      rake(verbose: false).invoke_task('site:build')
      o = Fronde::Org::File.new('public_html/index.html')
      data = o.to_h
      content = <<~CONTENT.strip
        <div id="content" class="content">
        <p>
        Nice content.
        </p>
        </div>
      CONTENT
      expect(data['published_body']).to eq(content)
    end

    it 'extracts gemini body', :aggregate_failures do # rubocop:disable RSpec/ExampleLength
      old_conf = Fronde::CONFIG.settings.merge
      old_conf['sources'] = [
        { 'name' => 'org', 'path' => 'src', 'target' => '.',
          'type' => 'gemini' }
      ]
      Fronde::CONFIG.load_test(old_conf)
      Fronde::CONFIG.write_org_lisp_config

      # Before build
      o = Fronde::Org::File.new('src/index.org')
      data = o.to_h
      expect(data['published_body']).to eq('Nice description')

      rake.invoke_task('site:build')
      o = Fronde::Org::File.new('public_gmi/index.gmi')
      data = o.to_h
      content = <<~CONTENT
        # Index file

        Nice content.


        --
        📅 Last modification on __PUB_DATE__
        📝 Written by alice with __VERSION__

      CONTENT
      version_rx = 'GNU/Emacs [0-9.]+ \(Org mode [0-9.]+\), ' \
                   'and published with Fronde [0-9.]+'
      page_content = data['published_body'].gsub(
        /[FMSTW][a-z]+, [ADFJMNOS][a-z]+ \d{1,2}, \d{4} at \d{2}:\d{2}/,
        '__PUB_DATE__'
      ).gsub(/#{version_rx}/, '__VERSION__')
      expect(page_content).to eq(content)
    end

    it 'does not build again with successive call' do
      rake(verbose: false).invoke_task('site:build')
      old_content = File.read('public_html/index.html')
      custom_footer = {
        'org-html' => { 'html-postamble' => '<footer>Modified!</footer>' }
      }
      new_conf = Fronde::CONFIG.settings.merge(custom_footer)
      Fronde::CONFIG.load_test(new_conf)
      Fronde::CONFIG.write_org_lisp_config
      rake(verbose: false).invoke_task('site:build')
      expect(File.read('public_html/index.html')).to eql(old_content)
    end

    it 'builds again when called with force option' do
      rake(verbose: false).invoke_task('site:build')
      old_content = File.read('public_html/index.html')
      custom_footer = {
        'org-html' => { 'html-postamble' => '<footer>Modified!</footer>' }
      }
      new_conf = Fronde::CONFIG.settings.merge(custom_footer)
      Fronde::CONFIG.load_test(new_conf)
      Fronde::CONFIG.write_org_lisp_config
      rake(verbose: false).invoke_task('site:build[true]')
      expect(File.read('public_html/index.html')).not_to eql(old_content)
    end

    it 'does not build again a single file with successive call' do
      rake(verbose: false).invoke_task('site:build')
      old_content = File.read('public_html/index.html')
      custom_footer = {
        'org-html' => { 'html-postamble' => '<footer>Modified!</footer>' }
      }
      new_conf = Fronde::CONFIG.settings.merge(custom_footer)
      Fronde::CONFIG.load_test(new_conf)
      Fronde::CONFIG.write_org_lisp_config
      rake(verbose: false).invoke_task('site:build_file[src/index.org]')
      expect(File.read('public_html/index.html')).to eql(old_content)
    end

    it 'builds again a single file when called with force option' do
      rake(verbose: false).invoke_task('site:build')
      old_content = File.read('public_html/index.html')
      custom_footer = {
        'org-html' => { 'html-postamble' => '<footer>Modified!</footer>' }
      }
      new_conf = Fronde::CONFIG.settings.merge(custom_footer)
      Fronde::CONFIG.load_test(new_conf)
      Fronde::CONFIG.write_org_lisp_config
      rake(verbose: false).invoke_task('site:build_file[src/index.org, true]')
      expect(File.read('public_html/index.html')).not_to eql(old_content)
    end

    it 'fails gracefully when something goes wrong' do
      old_conf = Fronde::CONFIG.settings.merge
      old_conf['emacs'] = 'notemacsatall'
      Fronde::CONFIG.load_test(old_conf)
      Fronde::CONFIG.write_org_lisp_config
      expect { rake(verbose: false).invoke_task('site:build') }.to(
        output(/Aborting/).to_stderr
      )
    end

    it 'warns about orphaned files' do
      FileUtils.mkdir_p 'public_html'
      File.write('public_html/orphan.html', 'Orphan content')
      expect { rake.invoke_task('site:build') }.to(
        output(
          %r{No project found for .+/orphan.html. Publication will fail.}
        ).to_stderr
      )
    end

    it 'warns about orphaned tag files' do
      old_conf = Fronde::CONFIG.settings.merge
      old_conf['sources'] = [
        { 'name' => 'org', 'path' => 'src', 'target' => '.',
          'is_blog' => true }
      ]
      Fronde::CONFIG.load_test(old_conf)
      Fronde::CONFIG.write_org_lisp_config
      FileUtils.mkdir_p 'src/tags'
      FileUtils.touch ['src/tags/test.org', 'src/tags/index.org']
      allow($stdin).to receive(:gets).and_return("n\n")
      alert = 'The file ./src/tags/test.org refers to a tag, ' \
              'which is no more in use.'
      expect { rake.invoke_task('site:clean') }.to(
        output(/#{alert}/).to_stdout
      )
    end

    it 'cleans orphaned files', :aggregate_failures do
      allow($stdin).to receive(:gets).and_return("y\n")
      FileUtils.mkdir_p 'public_html'
      File.write('public_html/orphan.html', 'Orphan content')
      rake.invoke_task('site:build')
      expect(File.exist?('public_html/orphan.html')).to be true
      rake.invoke_task('site:clean')
      expect(File.exist?('public_html/orphan.html')).to be false
    end

    it 'skips cleaning if answer is not y', :aggregate_failures do
      allow($stdin).to receive(:gets).and_return("nope\n")
      FileUtils.mkdir_p 'public_html'
      File.write('public_html/orphan.html', 'Orphan content')
      rake.invoke_task('site:build')
      expect(File.exist?('public_html/orphan.html')).to be true
      rake.invoke_task('site:clean')
      expect(File.exist?('public_html/orphan.html')).to be true
    end
  end

  context 'when generating indexes' do
    before { write_base_files }

    let(:whole_file_list) do
      ['src/news/index.org',
       'src/news/tags/index.org',
       'src/news/tags/toto.org',
       'src/news/tags/titi.org',
       'public_html/news/index.html',
       'public_html/news/tags/index.html',
       'public_html/news/tags/index.html',
       'public_html/news/tags/toto.html',
       'public_html/news/tags/titi.html',
       'public_html/news/feeds/index.xml',
       'public_html/news/feeds/toto.xml',
       'public_html/news/feeds/titi.xml']
    end

    context 'without blog setting' do
      before do
        old_conf = Fronde::CONFIG.settings.merge
        old_conf['sources'][1]['is_blog'] = false
        Fronde::CONFIG.load_test(old_conf)
        Fronde::CONFIG.write_org_lisp_config
      end

      it 'does not generate index' do
        rake.invoke_task('site:build')
        whole_file_list.each { |path| expect(File.exist?(path)).to be false }
      end

      it 'does not list tags' do
        ['tags:name', 'tags:weight'].each do |task_name|
          expect { rake.invoke_task(task_name) }.not_to output.to_stdout
        end
      end
    end

    context 'with wrong path' do
      before do
        old_conf = Fronde::CONFIG.settings.merge
        old_conf['sources'][1]['path'] = 'src/test'
        Fronde::CONFIG.load_test(old_conf)
        Fronde::CONFIG.write_org_lisp_config
      end

      it 'still generates some index', :aggregate_failures do
        rake.invoke_task('site:build')
        ['src/test/index.org',
         'src/test/tags/index.org',
         'public_html/test/index.html',
         'public_html/test/tags/index.html',
         'public_html/test/feeds/index.xml'].each do |path|
          expect(File.exist?(path)).to be true
        end
        # The 2 following does not exist are there was nothing to parse
        ['src/test/tags/toto.org', 'src/test/tags/titi.org'].each do |path|
          expect(File.exist?(path)).to be false
        end
      end

      it 'does not list tags' do
        ['tags:name', 'tags:weight'].each do |task_name|
          expect { rake.invoke_task(task_name) }.not_to output.to_stdout
        end
      end
    end

    context 'with a correct blog path' do
      it 'generates indexes' do
        rake(verbose: false).invoke_task('site:build')
        whole_file_list.each { |path| expect(File.exist?(path)).to be true }
      end

      it 'generates indexes in verbose mode' do
        # For coverage
        rake(verbose: true).invoke_task('site:build')
        whole_file_list.each { |path| expect(File.exist?(path)).to be true }
      end

      it 'lists all tags by name' do
        expect { rake.invoke_task('tags:name') }.to(
          output("toto (2)\ntiti (1)\n").to_stdout
        )
      end

      it 'lists all tags by weight' do
        expect { rake.invoke_task('tags:weight') }.to(
          output("titi (1)\ntoto (2)\n").to_stdout
        )
      end
    end
  end
end
