# frozen_string_literal: true

require 'r18n-core'
require './lib/neruda/org_file'

R18n.set('en', File.expand_path('../locales', __dir__))

describe 'With working org files' do
  it 'should parse without date' do
    o = Neruda::OrgFile.new('./spec/data/test1.org')
    expect(o.title).to eq('My sweet article')
    expect(o.date).to be_nil
    expect(o.timekey).to eq('00000000000000')
    expect(o.format('%d - (%t)')).to eq(' - (My sweet article)')
  end

  it 'should parse with a partial date' do
    o = Neruda::OrgFile.new('./spec/data/test2.org')
    expect(o.title).to eq('My second article')
    expect(o.date).to eq(DateTime.strptime('2019-06-11 00:00:00', '%Y-%m-%d %H:%M:%S'))
    expect(o.timekey).to eq('20190611000000')
    expect(o.format('%d - (%t)')).to eq('<time datetime="2019-06-11T00:00:00+00:00">2019-06-11</time> - (My second article)')
  end

  it 'should parse with a complete date' do
    o = Neruda::OrgFile.new('./spec/data/test3.org')
    expect(o.title).to eq('My third article')
    expect(o.date).to eq(DateTime.strptime('2019-06-11 23:42:10', '%Y-%m-%d %H:%M:%S'))
    expect(o.timekey).to eq('20190611234210')
    expect(o.format('%d - (%t)')).to eq('<time datetime="2019-06-11T23:42:10+00:00">2019-06-11</time> - (My third article)')
  end
end
