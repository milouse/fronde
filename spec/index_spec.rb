# coding: utf-8
# frozen_string_literal: true

require 'r18n-core'
require './lib/neruda/index'

R18n.set('en', File.expand_path('../locales', __dir__))

SAMPLE_INDEX = <<~IDX1
  #+title: Blog
  #+author:

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

describe 'With working org files' do
  before(:all) do
    Neruda::Config.load_test('blog_title' => 'Blog')
    @index = Neruda::Index.new(['./spec/data/test1.org',
                                './spec/data/test2.org',
                                './spec/data/test3.org'])
  end

  it 'should generate a main index' do
    expect(@index.to_s).to eq(SAMPLE_INDEX.strip)
  end

  it 'should have generated two indexes' do
    expect(@index.entries.length).to eq(2)
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
