# frozen_string_literal: true

SAMPLE_ALL_INDEX = <<~INDEX.strip
  #+title: All tags
  #+author: Test
  #+language: en

  * By alphabetical order
  :PROPERTIES:
  :HTML_CONTAINER_CLASS: index-tags
  :UNNUMBERED: notoc
  :END:

  - [[http://perdu.com/tags/toto.html][toto]] (1)
  - [[http://perdu.com/tags/tutu.html][tutu]] (2)

  * By publication number
  :PROPERTIES:
  :HTML_CONTAINER_CLASS: index-tags
  :UNNUMBERED: notoc
  :END:

  - [[http://perdu.com/tags/tutu.html][tutu]] (2)
  - [[http://perdu.com/tags/toto.html][toto]] (1)
INDEX

SAMPLE_PROJECT_INDEX = <<~BLOG_IDX.strip
  #+title: writings
  #+author: Test
  #+language: en

  * 2019
  :PROPERTIES:
  :HTML_CONTAINER_CLASS: index-year
  :UNNUMBERED: notoc
  :END:

  - *[[http://perdu.com/other/test3.html][My third article]]* / Published on Tuesday 11th of June at 23:42
  - *[[http://perdu.com/test2.html][My second article]]* / Published on Tuesday 11th of June \\\\
    Lorem ipsum

  * Unsorted
  :PROPERTIES:
  :HTML_CONTAINER_CLASS: index-year
  :UNNUMBERED: notoc
  :END:

  - *[[http://perdu.com/test1.html][My sweet article]]*
BLOG_IDX

SAMPLE_NO_REC_INDEX = <<~INDEX.strip
  #+title: All tags
  #+author: Test
  #+language: en

  * By alphabetical order
  :PROPERTIES:
  :HTML_CONTAINER_CLASS: index-tags
  :UNNUMBERED: notoc
  :END:

  - [[http://perdu.com/tags/tutu.html][tutu]] (1)

  * By publication number
  :PROPERTIES:
  :HTML_CONTAINER_CLASS: index-tags
  :UNNUMBERED: notoc
  :END:

  - [[http://perdu.com/tags/tutu.html][tutu]] (1)
INDEX

SAMPLE_PROJECT_NO_REC_INDEX = <<~BLOG_IDX.strip
  #+title: writings
  #+author: Test
  #+language: en

  * 2019
  :PROPERTIES:
  :HTML_CONTAINER_CLASS: index-year
  :UNNUMBERED: notoc
  :END:

  - *[[http://perdu.com/test2.html][My second article]]* / Published on Tuesday 11th of June \\\\
    Lorem ipsum

  * Unsorted
  :PROPERTIES:
  :HTML_CONTAINER_CLASS: index-year
  :UNNUMBERED: notoc
  :END:

  - *[[http://perdu.com/test1.html][My sweet article]]*
BLOG_IDX

SAMPLE_ATOM = <<~ATOM.strip
  <?xml version="1.0" encoding="utf-8"?>
  <feed xmlns="http://www.w3.org/2005/Atom"
        xmlns:dc="http://purl.org/dc/elements/1.1/"
        xmlns:wfw="http://wellformedweb.org/CommentAPI/"
        xml:lang="en">

  <title>Blog</title>
  <link href="http://perdu.com/feeds/index.xml" rel="self" type="application/atom+xml"/>
  <link href="http://perdu.com" rel="alternate" type="text/html" title="Blog"/>
  <updated>%<date>s</updated>
  <author><name>Test</name></author>
  <id>urn:md5:75d53866bcb20465b3287cf237234464</id>
  <generator uri="https://git.umaneti.net/fronde/about/">Fronde</generator>

  <entry>
    <title>My third article</title>
    <link href="http://perdu.com/other/test3.html" rel="alternate" type="text/html"
          title="My third article"/>
    <id>urn:md5:8865383febd94ddf9df318267af5ae85</id>
    <published>2019-06-11T23:42:10+00:00</published>
    <author><name>Test</name></author>
    <dc:subject>toto</dc:subject><dc:subject>tutu</dc:subject>
    <content type="html"></content>
  </entry>

  <entry>
    <title>My second article</title>
    <link href="http://perdu.com/test2.html" rel="alternate" type="text/html"
          title="My second article"/>
    <id>urn:md5:123104bd8bb4c61e02a1e2a136e2fd6b</id>
    <published>2019-06-11T00:00:00+00:00</published>
    <author><name>Titi</name></author>
    <content type="html">Lorem ipsum</content>
  </entry>

  <entry>
    <title>My sweet article</title>
    <link href="http://perdu.com/test1.html" rel="alternate" type="text/html"
          title="My sweet article"/>
    <id>urn:md5:c47532bbb1e2883c902071591ae1ec9b</id>
    <published></published>
    <author><name>Test</name></author>
    <dc:subject>tutu</dc:subject>
    <content type="html"></content>
  </entry>
  </feed>
ATOM

describe Fronde::Index do
  context 'with blog settings' do
    context 'with working org files' do
      before do
        FileUtils.mkdir_p 'tmp/blog/writings/other'
        FileUtils.mkdir_p 'tmp/blog/output'
        FileUtils.cp(
          ['data/test1.org',
           'data/test2.org'],
          'tmp/blog/writings'
        )
        FileUtils.cp 'data/test3.org', 'tmp/blog/writings/other'
        Dir.chdir 'tmp/blog'
      end

      after do
        tear_down 'tmp/blog'
      end

      context 'with recursive config' do
        before do
          Fronde::Config.load_test(
            'title' => 'Blog',
            'author' => 'Test',
            'public_folder' => 'output',
            'domain' => 'http://perdu.com',
            'sources' => [
              { 'path' => 'writings',
                'target' => '.',
                'is_blog' => true }
            ]
          )
        end

        it 'has generated two indexes', core: true do
          index = described_class.new
          expect(index.entries.length).to eq(2)
          expect(index.empty?).to be(false)
        end

        it 'excludes specified pattern from indexes', core: true do
          old_conf = Fronde::Config.settings.dup
          old_conf['sources'][0]['exclude'] = 'test3\.org$'
          Fronde::Config.load_test(old_conf)
          index = described_class.new
          expect(index.entries.length).to eq(1)
          expect(index.entries.include?('toto')).to be(false)
        end

        it 'generates a main index', core: true do
          index = described_class.new
          expect(index.to_s).to eq(SAMPLE_ALL_INDEX)
          expect(index.to_s('writings', is_project: true)).to(
            eq(SAMPLE_PROJECT_INDEX)
          )
        end

        it 'generates an atom feed', core: true do
          now_str = DateTime.now.strftime('%Y-%m-%d %H:%M')
          index = described_class.new
          index_date_str = index.date.strftime('%Y-%m-%d %H:%M')
          expect(index_date_str).to eq(now_str)
          expect(index.to_atom).to(
            eq(format(SAMPLE_ATOM, date: index.date.rfc3339))
          )
        end

        it 'generates the right org-header' do
          index = described_class.new
          header = ['#+title: ', '#+author: Test', '#+language: en'].join("\n").strip
          expect(index.send(:org_header)).to eq(header)
          expect(index.send(:org_header, 'index')).to eq(header)
          expect(index.send(:org_header, 'Test', is_tag: true)).to eq(header)

          header = ['#+title: Blog', '#+author: Test', '#+language: en'].join("\n").strip
          expect(index.send(:org_header, is_tag: false)).to eq(header)
          expect(index.send(:org_header, 'index', is_tag: false)).to eq(header)

          header = ['#+title: Test', '#+author: Test', '#+language: en'].join("\n")
          expect(index.send(:org_header, 'Test', is_tag: false)).to eq(header.strip)

          header = ['#+title: tutu', '#+author: Test', '#+language: en'].join("\n").strip
          expect(index.send(:org_header, 'tutu')).to eq(header)
          expect(index.send(:org_header, 'tutu', is_tag: true)).to eq(header)
        end

        it 'lists tags by name', core: true do
          list = described_class.new.sort_by :name
          expect(list).to eql(['tutu (2)', 'toto (1)'])
        end

        it 'lists tags by weight', core: true do
          list = described_class.new.sort_by :weight
          expect(list).to eql(['toto (1)', 'tutu (2)'])
        end

        it 'raises an error if sort_by is called with bad argument', core: true do
          index = described_class.new
          expect { index.sort_by(:test) }.to raise_error(ArgumentError)
        end

        it 'correctly saves one index', core: true do
          described_class.new.write_org('index')
          expect(File.exist?('tags/index.org')).to be(true)
          expect(File.read('tags/index.org')).to eq(SAMPLE_ALL_INDEX)
        end

        it 'correctly saves one blog index', core: true do
          described_class.new.send(:write_all_blog_home, false)
          expect(File.exist?('writings/index.org')).to be(true)
          expect(File.read('writings/index.org')).to eq(SAMPLE_PROJECT_INDEX)
        end

        it 'correctly saves one blog index, even verbosely', core: true do
          described_class.new.send(:write_all_blog_home, true)
          expect(File.exist?('writings/index.org')).to be(true)
          expect(File.read('writings/index.org')).to eq(SAMPLE_PROJECT_INDEX)
        end

        it 'correctly saves one atom feed', core: true do
          index = described_class.new
          index.write_atom('index')
          expect(File.exist?('output/feeds/index.xml')).to be(true)
          expect(File.read('output/feeds/index.xml')).to(
            eq(format(SAMPLE_ATOM, date: index.date.rfc3339))
          )
        end

        it 'writes them all', core: true do
          described_class.new.write_all
          expect(File.exist?('writings/index.org')).to be(true)
          expect(File.exist?('tags/index.org')).to be(true)
          expect(File.exist?('tags/toto.org')).to be(true)
          expect(File.exist?('tags/tutu.org')).to be(true)
          expect(File.exist?('output/feeds/index.xml')).to be(true)
          expect(File.exist?('output/feeds/toto.xml')).to be(true)
          expect(File.exist?('output/feeds/tutu.xml')).to be(true)
        end
      end

      context 'without recursive config' do
        before do
          Fronde::Config.load_test(
            'title' => 'Blog',
            'author' => 'Test',
            'public_folder' => 'output',
            'domain' => 'http://perdu.com',
            'sources' => [
              { 'path' => 'writings',
                'target' => '.',
                'recursive' => false,
                'is_blog' => true }
            ]
          )
        end

        it 'has generated one index', core: true do
          index = described_class.new
          expect(index.entries.length).to eq(1)
          expect(index.empty?).to be(false)
        end

        it 'generates a main index', core: true do
          index = described_class.new
          expect(index.to_s).to eq(SAMPLE_NO_REC_INDEX)
          expect(index.to_s('writings', is_project: true)).to(
            eq(SAMPLE_PROJECT_NO_REC_INDEX)
          )
        end

        it 'writes them all', core: true do
          described_class.new.write_all
          expect(File.exist?('writings/index.org')).to be(true)
          expect(File.exist?('tags/index.org')).to be(true)
          expect(File.exist?('tags/toto.org')).to be(false)
          expect(File.exist?('tags/tutu.org')).to be(true)
          expect(File.exist?('output/feeds/index.xml')).to be(true)
          expect(File.exist?('output/feeds/toto.xml')).to be(false)
          expect(File.exist?('output/feeds/tutu.xml')).to be(true)
        end
      end
    end

    context 'without org files' do
      before do
        FileUtils.mkdir_p 'tmp/blog/writings'
        FileUtils.mkdir_p 'tmp/blog/output'
        Dir.chdir 'tmp/blog'
        Fronde::Config.load_test(
          'title' => 'Blog',
          'author' => 'Test',
          'public_folder' => 'output',
          'domain' => 'http://perdu.com',
          'sources' => [
            { 'path' => 'writings',
              'target' => '.',
              'is_blog' => true }
          ]
        )
      end

      after do
        tear_down 'tmp/blog'
      end

      it 'has generated no index', core: true do
        index = described_class.new
        expect(index.entries.length).to eq(0)
        expect(index.empty?).to be(true)
      end

      it 'generates a main index', core: true do
        index = described_class.new
        empty_index = <<~INDEX
          #+title: All tags
          #+author: Test
          #+language: en

          * By alphabetical order
          :PROPERTIES:
          :HTML_CONTAINER_CLASS: index-tags
          :UNNUMBERED: notoc
          :END:


          * By publication number
          :PROPERTIES:
          :HTML_CONTAINER_CLASS: index-tags
          :UNNUMBERED: notoc
          :END:
        INDEX
        expect(index.to_s).to eq(empty_index)
        empty_project_index = <<~INDEX.strip
          #+title: writings
          #+author: Test
          #+language: en
        INDEX
        expect(index.to_s('writings', is_project: true)).to(
          eq(empty_project_index)
        )
      end

      it 'generates an atom feed', core: true do
        now_str = DateTime.now.strftime('%Y-%m-%d %H:%M')
        index = described_class.new
        index_date_str = index.date.strftime('%Y-%m-%d %H:%M')
        expect(index_date_str).to eq(now_str)
        empty_atom = <<~ATOM.strip
          <?xml version="1.0" encoding="utf-8"?>
          <feed xmlns="http://www.w3.org/2005/Atom"
                xmlns:dc="http://purl.org/dc/elements/1.1/"
                xmlns:wfw="http://wellformedweb.org/CommentAPI/"
                xml:lang="en">

          <title>Blog</title>
          <link href="http://perdu.com/feeds/index.xml" rel="self" type="application/atom+xml"/>
          <link href="http://perdu.com" rel="alternate" type="text/html" title="Blog"/>
          <updated>%<date>s</updated>
          <author><name>Test</name></author>
          <id>urn:md5:75d53866bcb20465b3287cf237234464</id>
          <generator uri="https://git.umaneti.net/fronde/about/">Fronde</generator>
          </feed>
        ATOM
        expect(index.to_atom).to(
          eq(format(empty_atom, date: index.date.rfc3339))
        )
      end

      it 'correctly saves one index', core: true do
        described_class.new.write_org('index')
        expect(File.exist?('tags/index.org')).to be(true)
        empty_index = <<~INDEX
          #+title: All tags
          #+author: Test
          #+language: en

          * By alphabetical order
          :PROPERTIES:
          :HTML_CONTAINER_CLASS: index-tags
          :UNNUMBERED: notoc
          :END:


          * By publication number
          :PROPERTIES:
          :HTML_CONTAINER_CLASS: index-tags
          :UNNUMBERED: notoc
          :END:
        INDEX
        expect(File.read('tags/index.org')).to eq(empty_index)
      end

      it 'correctly saves one blog index', core: true do
        described_class.new.send(:write_all_blog_home, false)
        expect(File.exist?('writings/index.org')).to be(true)
        empty_project_index = <<~INDEX.strip
          #+title: writings
          #+author: Test
          #+language: en
        INDEX
        expect(File.read('writings/index.org')).to eq(empty_project_index)
      end

      it 'correctly saves one atom feed', core: true do
        index = described_class.new
        index.write_atom('index')
        expect(File.exist?('output/feeds/index.xml')).to be(true)
        empty_atom = <<~ATOM.strip
          <?xml version="1.0" encoding="utf-8"?>
          <feed xmlns="http://www.w3.org/2005/Atom"
                xmlns:dc="http://purl.org/dc/elements/1.1/"
                xmlns:wfw="http://wellformedweb.org/CommentAPI/"
                xml:lang="en">

          <title>Blog</title>
          <link href="http://perdu.com/feeds/index.xml" rel="self" type="application/atom+xml"/>
          <link href="http://perdu.com" rel="alternate" type="text/html" title="Blog"/>
          <updated>%<date>s</updated>
          <author><name>Test</name></author>
          <id>urn:md5:75d53866bcb20465b3287cf237234464</id>
          <generator uri="https://git.umaneti.net/fronde/about/">Fronde</generator>
          </feed>
        ATOM
        expect(File.read('output/feeds/index.xml')).to(
          eq(format(empty_atom, date: index.date.rfc3339))
        )
      end

      it 'writes them all', core: true do
        described_class.new.write_all
        expect(File.exist?('writings/index.org')).to be(true)
        expect(File.exist?('tags/index.org')).to be(true)
        expect(File.exist?('output/feeds/index.xml')).to be(true)
      end
    end
  end

  context 'without blog settings' do
    before do
      FileUtils.mkdir_p 'tmp/txt/writings'
      FileUtils.mkdir_p 'tmp/txt/output'
      FileUtils.cp(
        ['data/test1.org',
         'data/test2.org',
         'data/test3.org'],
        'tmp/txt/writings'
      )
      Dir.chdir 'tmp/txt'
      Fronde::Config.load_test(
        'title' => 'My site',
        'author' => 'Test',
        'public_folder' => 'output',
        'domain' => 'http://perdu.com',
        'sources' => [{ 'path' => 'writings', 'target' => '.' }]
      )
    end

    after do
      tear_down 'tmp/txt'
    end

    it 'does not have generated any indexes', core: true do
      index = described_class.new
      expect(index.entries.length).to eq(0)
      expect(index.empty?).to be(true)
    end

    it 'generates an empty main index', core: true do
      empty_index = <<~EMPTY_INDEX
        #+title: All tags
        #+author: Test
        #+language: en

        * By alphabetical order
        :PROPERTIES:
        :HTML_CONTAINER_CLASS: index-tags
        :UNNUMBERED: notoc
        :END:


        * By publication number
        :PROPERTIES:
        :HTML_CONTAINER_CLASS: index-tags
        :UNNUMBERED: notoc
        :END:
      EMPTY_INDEX
      expect(described_class.new.to_s).to eq(empty_index)
    end

    it 'generates an empty atom feed', core: true do
      now_str = DateTime.now.strftime('%Y-%m-%d %H:%M')
      index = described_class.new
      index_date_str = index.date.strftime('%Y-%m-%d %H:%M')
      expect(index_date_str).to eq(now_str)
      empty_atom = <<~EMPTY_ATOM.strip
        <?xml version="1.0" encoding="utf-8"?>
        <feed xmlns="http://www.w3.org/2005/Atom"
              xmlns:dc="http://purl.org/dc/elements/1.1/"
              xmlns:wfw="http://wellformedweb.org/CommentAPI/"
              xml:lang="en">

        <title>My site</title>
        <link href="http://perdu.com/feeds/index.xml" rel="self" type="application/atom+xml"/>
        <link href="http://perdu.com" rel="alternate" type="text/html" title="My site"/>
        <updated>%<date>s</updated>
        <author><name>Test</name></author>
        <id>urn:md5:75d53866bcb20465b3287cf237234464</id>
        <generator uri="https://git.umaneti.net/fronde/about/">Fronde</generator>
        </feed>
      EMPTY_ATOM
      expect(index.to_atom).to(
        eq(format(empty_atom, date: index.date.rfc3339))
      )
    end

    it 'does not save one index', core: true do
      described_class.new.write_org('index')
      expect(File.exist?('tags/index.org')).to be(false)
    end

    it 'does not save one atom feed', core: true do
      described_class.new.write_atom('index')
      expect(File.exist?('output/feeds/index.xml')).to be(false)
    end

    it 'does not write it all', core: true do
      described_class.new.write_all
      expect(File.exist?('writings/index.org')).to be(false)
      expect(File.exist?('tags/index.org')).to be(false)
      expect(File.exist?('tags/toto.org')).to be(false)
      expect(File.exist?('tags/tutu.org')).to be(false)
      expect(File.exist?('output/feeds/index.xml')).to be(false)
      expect(File.exist?('output/feeds/toto.xml')).to be(false)
      expect(File.exist?('output/feeds/tutu.xml')).to be(false)
    end
  end

  context 'without neither blog settings nor site title' do
    before do
      FileUtils.mkdir_p 'tmp/txt/writings'
      FileUtils.mkdir_p 'tmp/txt/output'
      FileUtils.cp 'data/test1.org', 'tmp/txt/writings'
      Dir.chdir 'tmp/txt'
      Fronde::Config.load_test(
        'public_folder' => 'output',
        'domain' => 'http://perdu.com',
        'sources' => [{ 'path' => 'writings', 'target' => '.' }]
      )
    end

    after do
      tear_down 'tmp/txt'
    end

    it 'does not have generated any indexes', core: true do
      index = described_class.new
      expect(index.entries.length).to eq(0)
      expect(index.empty?).to be(true)
    end

    it 'generates an empty main index', core: true do
      empty_index = <<~EMPTY_INDEX
        #+title: All tags
        #+author: alice
        #+language: en

        * By alphabetical order
        :PROPERTIES:
        :HTML_CONTAINER_CLASS: index-tags
        :UNNUMBERED: notoc
        :END:


        * By publication number
        :PROPERTIES:
        :HTML_CONTAINER_CLASS: index-tags
        :UNNUMBERED: notoc
        :END:
      EMPTY_INDEX
      expect(described_class.new.to_s).to eq(empty_index)
    end

    it 'generates an empty atom feed', core: true do
      now_str = DateTime.now.strftime('%Y-%m-%d %H:%M')
      index = described_class.new
      index_date_str = index.date.strftime('%Y-%m-%d %H:%M')
      expect(index_date_str).to eq(now_str)
      empty_atom = <<~EMPTY_ATOM.strip
        <?xml version="1.0" encoding="utf-8"?>
        <feed xmlns="http://www.w3.org/2005/Atom"
              xmlns:dc="http://purl.org/dc/elements/1.1/"
              xmlns:wfw="http://wellformedweb.org/CommentAPI/"
              xml:lang="en">

        <title>All tags</title>
        <link href="http://perdu.com/feeds/index.xml" rel="self" type="application/atom+xml"/>
        <link href="http://perdu.com" rel="alternate" type="text/html" title="All tags"/>
        <updated>%<date>s</updated>
        <author><name>alice</name></author>
        <id>urn:md5:75d53866bcb20465b3287cf237234464</id>
        <generator uri="https://git.umaneti.net/fronde/about/">Fronde</generator>
        </feed>
      EMPTY_ATOM
      expect(index.to_atom).to(
        eq(format(empty_atom, date: index.date.rfc3339))
      )
    end

    it 'does not save any index', core: true do
      index = described_class.new
      index.write_org('index')
      expect(File.exist?('tags/index.org')).to be(false)
      index.write_atom('index')
      expect(File.exist?('output/feeds/index.xml')).to be(false)
    end
  end
end
