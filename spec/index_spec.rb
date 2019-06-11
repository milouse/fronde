# coding: utf-8
# frozen_string_literal: true

require 'r18n-core'
require './lib/neruda/index'

R18n.set('en', File.expand_path('../locales', __dir__))

SAMPLE_INDEX = <<~IDX1
  #+title: Blog
  #+author: Test

  * 2019
  :PROPERTIES:
  :UNNUMBERED: notoc
  :END:

  - 2019-06-11: [[./data][My third article]]
  - 2019-06-11: [[./data][My second article]]

  * 0000
  :PROPERTIES:
  :UNNUMBERED: notoc
  :END:

  - : [[./data][My sweet article]]
IDX1

SAMPLE_ATOM = <<~ATOM
  <?xml version="1.0" encoding="utf-8"?>
  <feed xmlns="http://www.w3.org/2005/Atom"
        xmlns:dc="http://purl.org/dc/elements/1.1/"
        xmlns:wfw="http://wellformedweb.org/CommentAPI/"
        xml:lang="en">

  <title>index</title>
  <link href="/atom.xml" rel="self" type="application/atom+xml"/>
  <link href="" rel="alternate" type="text/html" title="index"/>
  <updated>---testupdate---</updated>
  <author><name>Test</name></author>
  <id>urn:md5:d41d8cd98f00b204e9800998ecf8427e</id>
  <generator uri="https://fossil.deparis.io/neruda">Neruda</generator>

  <entry>
    <title>My third article</title>
    <link href="./spec/data/test3.org" rel="alternate" type="text/html"
          title="My third article"/>
    <id>urn:md5:8865383febd94ddf9df318267af5ae85</id>
    <published>2019-06-11T23:42:10+00:00</published>
    <author><name>Test</name></author>
    <dc:subject>toto</dc:subject><dc:subject>tutu</dc:subject>
    <content type="html"></content>
  </entry>

  <entry>
    <title>My second article</title>
    <link href="./spec/data/test2.org" rel="alternate" type="text/html"
          title="My second article"/>
    <id>urn:md5:123104bd8bb4c61e02a1e2a136e2fd6b</id>
    <published>2019-06-11T00:00:00+00:00</published>
    <author><name>Titi</name></author>
    <content type="html"></content>
  </entry>

  <entry>
    <title>My sweet article</title>
    <link href="./spec/data/test1.org" rel="alternate" type="text/html"
          title="My sweet article"/>
    <id>urn:md5:c47532bbb1e2883c902071591ae1ec9b</id>
    <published></published>
    <author><name>Test</name></author>
    <dc:subject>toto</dc:subject>
    <content type="html"></content>
  </entry>
  </feed>
ATOM

describe 'With working org files' do
  before(:all) do
    Neruda::Config.load_test('blog_title' => 'Blog', 'author' => 'Test')
    @index = Neruda::Index.new(['./spec/data/test1.org',
                                './spec/data/test2.org',
                                './spec/data/test3.org'])
  end

  it 'should have generated three indexes' do
    expect(@index.entries.length).to eq(3)
  end

  it 'should generate a main index' do
    expect(@index.to_s).to eq(SAMPLE_INDEX.strip)
  end

  it 'should generate an atom feed' do
    expect(@index.to_atom).to eq(SAMPLE_ATOM.strip)
  end
end

describe 'With various tag names' do
  it 'should transliterate them' do
    expect(Neruda::Index.slug('toto')).to eq('toto')
    expect(Neruda::Index.slug('TotO')).to eq('toto')
    expect(Neruda::Index.slug('Tôto')).to eq('toto')
    expect(Neruda::Index.slug('Tôto tata')).to eq('toto-tata')
    expect(Neruda::Index.slug('ÀùïỸç/+*= trulu°`')).to eq('auiyc-trulu')
  end
end
