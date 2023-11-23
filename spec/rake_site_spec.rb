# frozen_string_literal: true

require 'rake'

def write_base_files # rubocop:disable Metrics/MethodLength
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
  FileUtils.mkdir_p 'src/news'
  org_content = <<~ORG
    #+title: Index file

    My website
  ORG
  File.write('src/index.org', org_content)
  file1 = <<~ORG
    #+title: Index file
    #+keywords: toto, titi

    My website
  ORG
  File.write('src/news/test1.org', file1)
  file2 = <<~ORG
    #+title: Index file
    #+keywords: toto

    My website
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
      o = Fronde::Org::File.new(
        'src/index.org',
        title: 'My website',
        content: "#+description: Nice description\n\nNice content."
      )
      o.write
    end

    it 'builds something' do
      rake(verbose: false).invoke_task('site:build')
      expect(File.exist?('public_html/index.html')).to be(true)
    end

    it 'builds something even in verbose mode' do
      # For coverage
      rake(verbose: true).invoke_task('site:build')
      expect(File.exist?('public_html/index.html')).to be(true)
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
      expect(File.exist?('public_gmi/index.gmi')).to be(true)
    end

    it 'extracts html body' do
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

    it 'extracts gemini body' do
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
        # My website

        Nice content.


        --
        📅 Last modification on __PUB_DATE__
        📝 Written by alice with __VERSION__

      CONTENT
      page_content = data['published_body'].gsub(
        /[FMSTW][a-z]+ \d{1,2} of [ADFJMNOS][a-z]+, \d{4} at \d{2}:\d{2}/,
        '__PUB_DATE__'
      ).gsub(
        /GNU\/Emacs [0-9.]+ \(Org mode [0-9.]+\), and published with Fronde [0-9.]+/,
        '__VERSION__'
      )
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

    it 'builds again when call with force option' do
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
          /No project found for .+\/orphan.html. Publication will fail./
        ).to_stderr
      )
    end

    it 'cleans orphaned files' do
      allow($stdin).to receive(:gets).and_return("y\n")
      FileUtils.mkdir_p 'public_html'
      File.write('public_html/orphan.html', 'Orphan content')
      rake.invoke_task('site:build')
      expect(File.exist?('public_html/orphan.html')).to be true
      rake.invoke_task('site:clean')
      expect(File.exist?('public_html/orphan.html')).to be false
    end

    it 'skips cleaning if answer is not y' do
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

    context 'without blog setting' do
      before do
        old_conf = Fronde::CONFIG.settings.merge
        old_conf['sources'][1]['is_blog'] = false
        Fronde::CONFIG.load_test(old_conf)
        Fronde::CONFIG.write_org_lisp_config
      end

      it 'does not generate index' do
        rake.invoke_task('site:build')
        expect(File.exist?('src/news/index.org')).to be(false)
        expect(File.exist?('src/news/tags/index.org')).to be(false)
        expect(File.exist?('src/news/tags/toto.org')).to be(false)
        expect(File.exist?('src/news/tags/titi.org')).to be(false)
        expect(File.exist?('public_html/news/index.html')).to be(false)
        expect(File.exist?('public_html/news/tags/index.html')).to be(false)
        expect(File.exist?('public_html/news/tags/index.html')).to be(false)
        expect(File.exist?('public_html/news/tags/toto.html')).to be(false)
        expect(File.exist?('public_html/news/tags/titi.html')).to be(false)
        expect(File.exist?('public_html/news/feeds/index.xml')).to be(false)
        expect(File.exist?('public_html/news/feeds/toto.xml')).to be(false)
        expect(File.exist?('public_html/news/feeds/titi.xml')).to be(false)
      end

      it 'does not list tags' do
        expect { rake.invoke_task('tags:name') }.to output('').to_stdout
        expect { rake.invoke_task('tags:weight') }.to output('').to_stdout
      end
    end

    context 'with wrong path' do
      before do
        old_conf = Fronde::CONFIG.settings.merge
        old_conf['sources'][1]['path'] = 'src/test'
        Fronde::CONFIG.load_test(old_conf)
        Fronde::CONFIG.write_org_lisp_config
      end

      it 'still generates some index' do
        rake.invoke_task('site:build')
        expect(File.exist?('src/test/index.org')).to be(true)
        expect(File.exist?('src/test/tags/index.org')).to be(true)
        # The 2 following does not exist are there was nothing to parse
        expect(File.exist?('src/test/tags/toto.org')).to be(false)
        expect(File.exist?('src/test/tags/titi.org')).to be(false)
        expect(File.exist?('public_html/test/index.html')).to be(true)
        expect(File.exist?('public_html/test/tags/index.html')).to be(true)
        expect(File.exist?('public_html/test/feeds/index.xml')).to be(true)
      end

      it 'does not list tags' do
        expect { rake.invoke_task('tags:name') }.to output('').to_stdout
        expect { rake.invoke_task('tags:weight') }.to output('').to_stdout
      end
    end

    context 'with a correct blog path' do
      it 'generates indexes' do
        rake(verbose: false).invoke_task('site:build')
        expect(File.exist?('src/news/index.org')).to be(true)
        expect(File.exist?('src/news/tags/index.org')).to be(true)
        expect(File.exist?('src/news/tags/toto.org')).to be(true)
        expect(File.exist?('src/news/tags/titi.org')).to be(true)
        expect(File.exist?('public_html/news/index.html')).to be(true)
        expect(File.exist?('public_html/news/tags/index.html')).to be(true)
        expect(File.exist?('public_html/news/tags/index.html')).to be(true)
        expect(File.exist?('public_html/news/tags/toto.html')).to be(true)
        expect(File.exist?('public_html/news/tags/titi.html')).to be(true)
        expect(File.exist?('public_html/news/feeds/index.xml')).to be(true)
        expect(File.exist?('public_html/news/feeds/toto.xml')).to be(true)
        expect(File.exist?('public_html/news/feeds/titi.xml')).to be(true)
      end

      it 'generates indexes in verbose mode' do
        # For coverage
        rake(verbose: true).invoke_task('site:build')
        expect(File.exist?('src/news/index.org')).to be(true)
        expect(File.exist?('src/news/tags/index.org')).to be(true)
        expect(File.exist?('src/news/tags/toto.org')).to be(true)
        expect(File.exist?('src/news/tags/titi.org')).to be(true)
        expect(File.exist?('public_html/news/index.html')).to be(true)
        expect(File.exist?('public_html/news/tags/index.html')).to be(true)
        expect(File.exist?('public_html/news/tags/index.html')).to be(true)
        expect(File.exist?('public_html/news/tags/toto.html')).to be(true)
        expect(File.exist?('public_html/news/tags/titi.html')).to be(true)
        expect(File.exist?('public_html/news/feeds/index.xml')).to be(true)
        expect(File.exist?('public_html/news/feeds/toto.xml')).to be(true)
        expect(File.exist?('public_html/news/feeds/titi.xml')).to be(true)
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
