# frozen_string_literal: true

SAMPLE_INDEX = <<~IDX1
  #+title: Blog
  #+author: Test
  #+language: en

  * 2019
  :PROPERTIES:
  :HTML_CONTAINER_CLASS: index-year
  :UNNUMBERED: notoc
  :END:

  - *[[../data/test3.html][My third article]]* / Published on Tuesday 11th of June at 23:42
  - *[[../data/test2.html][My second article]]* / Published on Tuesday 11th of June

  * Unsorted
  :PROPERTIES:
  :HTML_CONTAINER_CLASS: index-year
  :UNNUMBERED: notoc
  :END:

  - *[[../data/test1.html][My sweet article]]*
IDX1

SAMPLE_ATOM = <<~ATOM
  <?xml version="1.0" encoding="utf-8"?>
  <feed xmlns="http://www.w3.org/2005/Atom"
        xmlns:dc="http://purl.org/dc/elements/1.1/"
        xmlns:wfw="http://wellformedweb.org/CommentAPI/"
        xml:lang="en">

  <title>Blog</title>
  <link href="http://perdu.com/feeds/index.xml" rel="self" type="application/atom+xml"/>
  <link href="http://perdu.com/blog" rel="alternate" type="text/html" title="Blog"/>
  <updated>%s</updated>
  <author><name>Test</name></author>
  <id>urn:md5:75d53866bcb20465b3287cf237234464</id>
  <generator uri="https://fossil.deparis.io/neruda">Neruda</generator>

  <entry>
    <title>My third article</title>
    <link href="http://perdu.com/data/test3.html" rel="alternate" type="text/html"
          title="My third article"/>
    <id>urn:md5:8865383febd94ddf9df318267af5ae85</id>
    <published>2019-06-11T23:42:10+00:00</published>
    <author><name>Test</name></author>
    <dc:subject>toto</dc:subject><dc:subject>tutu</dc:subject>
    <content type="html"></content>
  </entry>

  <entry>
    <title>My second article</title>
    <link href="http://perdu.com/data/test2.html" rel="alternate" type="text/html"
          title="My second article"/>
    <id>urn:md5:123104bd8bb4c61e02a1e2a136e2fd6b</id>
    <published>2019-06-11T00:00:00+00:00</published>
    <author><name>Titi</name></author>
    <content type="html"></content>
  </entry>

  <entry>
    <title>My sweet article</title>
    <link href="http://perdu.com/data/test1.html" rel="alternate" type="text/html"
          title="My sweet article"/>
    <id>urn:md5:c47532bbb1e2883c902071591ae1ec9b</id>
    <published></published>
    <author><name>Test</name></author>
    <dc:subject>tutu</dc:subject>
    <content type="html"></content>
  </entry>
  </feed>
ATOM

describe 'With blog settings' do
  describe 'with working org files' do
    before(:each) do
      Neruda::Config.load_test('title' => 'Blog',
                               'author' => 'Test',
                               'public_path' => 'spec/data/indexes',
                               'blog_path' => 'blog',
                               'domain' => 'http://perdu.com')
      @now = DateTime.now
      @index = Neruda::Index.new(['spec/data/test1.org',
                                  'spec/data/test2.org',
                                  'spec/data/test3.org'])
    end

    it 'should have generated three indexes', core: true do
      expect(@index.entries.length).to eq(3)
    end

    it 'should generate a main index', core: true do
      expect(@index.to_s).to eq(SAMPLE_INDEX.strip)
    end

    it 'should generate an atom feed', core: true do
      index_date_str = @index.date.strftime('%Y-%m-%d %H:%M')
      expect(index_date_str).to eq(@now.strftime('%Y-%m-%d %H:%M'))
      expect(@index.to_atom).to eq(SAMPLE_ATOM.strip % @index.date.rfc3339)
    end

    describe 'when trying to save them' do
      before(:each) do
        FileUtils.mkdir_p 'spec/data/indexes/src/blog'
        Dir.chdir 'spec/data/indexes'
      end

      after(:each) do
        Dir.chdir File.expand_path('../', __dir__)
        FileUtils.rm_r 'spec/data/indexes', force: true
      end

      it 'should correctly save one index', core: true do
        @index.write_org('index')
        expect(File.exist?('src/blog/index.org')).to be(true)
        expect(IO.read('src/blog/index.org')).to \
          eq(SAMPLE_INDEX.strip)
      end

      it 'should correctly save one atom feed', core: true do
        @index.write_atom('index')
        expect(File.exist?('public_html/feeds/index.xml')).to be(true)
        expect(IO.read('public_html/feeds/index.xml')).to \
          eq(SAMPLE_ATOM.strip % @index.date.rfc3339)
      end

      it 'should write it all', core: true do
        @index.write_all
        expect(File.exist?('src/blog/index.org')).to be(true)
        expect(File.exist?('src/tags/toto.org')).to be(true)
        expect(File.exist?('src/tags/tutu.org')).to be(true)
        expect(File.exist?('public_html/feeds/index.xml')).to be(true)
        expect(File.exist?('public_html/feeds/toto.xml')).to be(true)
        expect(File.exist?('public_html/feeds/tutu.xml')).to be(true)
      end

      it 'should list tags by name', core: true do
        list = @index.sort_by :name
        expect(list).to eql(['toto', 'tutu'])
      end

      it 'should list tags by weight', core: true do
        list = @index.sort_by :weight
        expect(list).to eql(['tutu', 'toto'])
      end

      it 'should raise an error if sort_by is called with bad argument', core: true do
        expect { @index.sort_by(:test) }.to raise_error(ArgumentError)
      end
    end
  end
end

describe 'Without blog settings' do
  describe 'with working org files' do
    before(:each) do
      Neruda::Config.load_test('title' => 'My site',
                               'author' => 'Test',
                               'public_path' => 'spec/data/indexes',
                               'domain' => 'http://perdu.com')
      @now = DateTime.now
      @index = Neruda::Index.new(['spec/data/test1.org',
                                  'spec/data/test2.org',
                                  'spec/data/test3.org'])
    end

    it 'should not have generated three indexes', core: true do
      expect(@index.entries.length).to eq(1) # default index one
    end

    it 'should generate an empty main index', core: true do
      empty_index = <<~EMPTY_INDEX
        #+title: My site
        #+author: Test
        #+language: en
      EMPTY_INDEX
      expect(@index.to_s).to eq(empty_index.strip)
    end

    it 'should generate an empty atom feed', core: true do
      index_date_str = @index.date.strftime('%Y-%m-%d %H:%M')
      expect(index_date_str).to eq(@now.strftime('%Y-%m-%d %H:%M'))
      empty_atom = <<~EMPTY_ATOM
        <?xml version="1.0" encoding="utf-8"?>
        <feed xmlns="http://www.w3.org/2005/Atom"
              xmlns:dc="http://purl.org/dc/elements/1.1/"
              xmlns:wfw="http://wellformedweb.org/CommentAPI/"
              xml:lang="en">

        <title>My site</title>
        <link href="http://perdu.com/feeds/index.xml" rel="self" type="application/atom+xml"/>
        <link href="http://perdu.com/" rel="alternate" type="text/html" title="My site"/>
        <updated>%s</updated>
        <author><name>Test</name></author>
        <id>urn:md5:75d53866bcb20465b3287cf237234464</id>
        <generator uri="https://fossil.deparis.io/neruda">Neruda</generator>
        </feed>
      EMPTY_ATOM
      expect(@index.to_atom).to eq(empty_atom.strip % @index.date.rfc3339)
    end

    describe 'when trying to save them' do
      before(:each) do
        FileUtils.mkdir_p 'spec/data/indexes/src/blog'
        Dir.chdir 'spec/data/indexes'
      end

      after(:each) do
        Dir.chdir File.expand_path('../', __dir__)
        FileUtils.rm_r 'spec/data/indexes', force: true
      end

      it 'should not save one index', core: true do
        @index.write_org('index')
        expect(File.exist?('src/blog/index.org')).to be(false)
      end

      it 'should not save one atom feed', core: true do
        @index.write_atom('index')
        expect(File.exist?('public_html/feeds/index.xml')).to be(false)
      end

      it 'should not write it all', core: true do
        @index.write_all
        expect(File.exist?('src/blog/index.org')).to be(false)
        expect(File.exist?('src/tags/toto.org')).to be(false)
        expect(File.exist?('src/tags/tutu.org')).to be(false)
        expect(File.exist?('public_html/feeds/index.xml')).to be(false)
        expect(File.exist?('public_html/feeds/toto.xml')).to be(false)
        expect(File.exist?('public_html/feeds/tutu.xml')).to be(false)
      end
    end
  end
end
