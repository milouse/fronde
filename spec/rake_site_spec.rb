# frozen_string_literal: true

require 'rake'

def write_base_files
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
  before do
    init_testing_website
    copy_org_tarball_to_fake_tmp
    Fronde::Config.send(:load_settings)
    rake.invoke_task('org:install')
  end

  after do
    Dir.chdir File.expand_path('..', __dir__)
    FileUtils.rm_r 'tmp/website_testing', force: true
  end

  context 'when building org files' do
    before do
      o = Fronde::OrgFile.new('src/index.org',
                              title: 'My website',
                              content: 'Nice content.')
      o.write
    end

    it 'builds something', rake: true do
      rake(verbose: false).invoke_task('site:build')
      expect(File.exist?('public_html/index.html')).to be(true)
    end

    it 'builds something even in verbose mode', rake: true do
      rake(verbose: true).invoke_task('site:build')
      expect(File.exist?('public_html/index.html')).to be(true)
    end

    it 'does not build again with successive call', rake: true do
      rake(verbose: false).invoke_task('site:build')
      old_content = File.read('public_html/index.html')
      old_conf = Fronde::Config.settings.dup
      old_conf['org-html']['html-postamble'] = '<footer>Modified!</footer>'
      Fronde::Config.load_test(old_conf)
      rake(verbose: false).invoke_task('site:build')
      expect(File.read('public_html/index.html')).to eql(old_content)
    end

    it 'builds again when call with force option', rake: true do
      rake(verbose: false).invoke_task('site:build')
      old_content = File.read('public_html/index.html')
      old_conf = Fronde::Config.settings.dup
      old_conf['org-html']['html-postamble'] = '<footer>Modified!</footer>'
      Fronde::Config.load_test(old_conf)
      rake(verbose: false).invoke_task('site:build[true]')
      expect(File.read('public_html/index.html')).not_to eql(old_content)
    end

    it 'fails gracefully when something goes wrong', rake: true do
      old_conf = Fronde::Config.settings.dup
      old_conf['emacs'] = 'notemacsatall'
      Fronde::Config.load_test(old_conf)
      expect { rake(verbose: false).invoke_task('site:build') }.to(
        output(/Aborting/).to_stderr
      )
    end
  end

  context 'when generating indexes' do
    before { write_base_files }

    context 'without blog setting', rake: true do
      before do
        old_conf = Fronde::Config.settings.dup
        old_conf['sources'][1]['is_blog'] = false
        Fronde::Config.load_test(old_conf)
      end

      it 'does not generate index', rake: true do
        rake.invoke_task('site:index')
        expect(File.exist?('src/news/index.org')).to be(false)
        expect(File.exist?('tags/index.org')).to be(false)
        expect(File.exist?('tags/toto.org')).to be(false)
        expect(File.exist?('tags/titi.org')).to be(false)
        expect(File.exist?('public_html/feeds/index.xml')).to be(false)
        expect(File.exist?('public_html/feeds/toto.xml')).to be(false)
        expect(File.exist?('public_html/feeds/titi.xml')).to be(false)
      end

      it 'does not generate index when calling build', rake: true do
        rake.invoke_task('site:build')
        expect(File.exist?('src/news/index.org')).to be(false)
        expect(File.exist?('tags/index.org')).to be(false)
        expect(File.exist?('tags/toto.org')).to be(false)
        expect(File.exist?('tags/titi.org')).to be(false)
        expect(File.exist?('public_html/news/index.html')).to be(false)
        expect(File.exist?('public_html/tags/index.html')).to be(false)
        expect(File.exist?('public_html/tags/index.html')).to be(false)
        expect(File.exist?('public_html/tags/toto.html')).to be(false)
        expect(File.exist?('public_html/tags/titi.html')).to be(false)
        expect(File.exist?('public_html/feeds/index.xml')).to be(false)
        expect(File.exist?('public_html/feeds/toto.xml')).to be(false)
        expect(File.exist?('public_html/feeds/titi.xml')).to be(false)
      end

      it 'does not list tags', rake: true do
        expect { rake.invoke_task('tags:name') }.to output('').to_stdout
        expect { rake.invoke_task('tags:weight') }.to output('').to_stdout
      end
    end

    context 'with wrong blog settings', rake: true do
      before do
        old_conf = Fronde::Config.settings.dup
        old_conf['sources'][1]['path'] = 'src/test'
        Fronde::Config.load_test(old_conf)
      end

      it 'does not generate index', rake: true do
        rake.invoke_task('site:index')
        expect(File.exist?('src/test/index.org')).to be(false)
        expect(File.exist?('public_html/feeds/index.xml')).to be(false)
      end

      it 'does not generate index when calling build', rake: true do
        rake.invoke_task('site:build')
        expect(File.exist?('src/test/index.org')).to be(false)
        expect(File.exist?('tags/index.org')).to be(false)
        expect(File.exist?('tags/toto.org')).to be(false)
        expect(File.exist?('tags/titi.org')).to be(false)
        expect(File.exist?('public_html/test/index.html')).to be(false)
        expect(File.exist?('public_html/tags/index.html')).to be(false)
        expect(File.exist?('public_html/feeds/index.xml')).to be(false)
      end

      it 'does not list tags', rake: true do
        expect { rake.invoke_task('tags:name') }.to output('').to_stdout
        expect { rake.invoke_task('tags:weight') }.to output('').to_stdout
      end
    end

    context 'with a correct blog path' do
      it 'generates indexes', rake: true do
        rake.invoke_task('site:index')
        expect(File.exist?('src/news/index.org')).to be(true)
        expect(File.exist?('tags/index.org')).to be(true)
        expect(File.exist?('tags/toto.org')).to be(true)
        expect(File.exist?('tags/titi.org')).to be(true)
        expect(File.exist?('public_html/feeds/index.xml')).to be(true)
        expect(File.exist?('public_html/feeds/toto.xml')).to be(true)
        expect(File.exist?('public_html/feeds/titi.xml')).to be(true)
      end

      it 'generates indexes, even verbosely', rake: true do
        rake(verbose: true).invoke_task('site:index')
        expect(File.exist?('src/news/index.org')).to be(true)
        expect(File.exist?('tags/index.org')).to be(true)
        expect(File.exist?('tags/toto.org')).to be(true)
        expect(File.exist?('tags/titi.org')).to be(true)
        expect(File.exist?('public_html/feeds/index.xml')).to be(true)
        expect(File.exist?('public_html/feeds/toto.xml')).to be(true)
        expect(File.exist?('public_html/feeds/titi.xml')).to be(true)
      end

      it 'generates indexes, even with build', rake: true do
        rake.invoke_task('site:build')
        expect(File.exist?('src/news/index.org')).to be(true)
        expect(File.exist?('tags/index.org')).to be(true)
        expect(File.exist?('tags/toto.org')).to be(true)
        expect(File.exist?('tags/titi.org')).to be(true)
        expect(File.exist?('public_html/news/index.html')).to be(true)
        expect(File.exist?('public_html/tags/index.html')).to be(true)
        expect(File.exist?('public_html/tags/index.html')).to be(true)
        expect(File.exist?('public_html/tags/toto.html')).to be(true)
        expect(File.exist?('public_html/tags/titi.html')).to be(true)
        expect(File.exist?('public_html/feeds/index.xml')).to be(true)
        expect(File.exist?('public_html/feeds/toto.xml')).to be(true)
        expect(File.exist?('public_html/feeds/titi.xml')).to be(true)
      end

      it 'lists all tags by name', rake: true do
        expect { rake.invoke_task('tags:name') }.to(
          output("toto (2)\ntiti (1)\n").to_stdout
        )
      end

      it 'lists all tags by weight', rake: true do
        expect { rake.invoke_task('tags:weight') }.to(
          output("titi (1)\ntoto (2)\n").to_stdout
        )
      end
    end
  end
end
