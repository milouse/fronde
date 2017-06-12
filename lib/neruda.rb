# coding: utf-8
# frozen_string_literal: true

require 'yaml'
require 'org-ruby'
require 'sinatra/base'

# Namespacing
module Neruda
  CONFIG = YAML.load_file(File.join('config', 'config.yml')).freeze
end

# The following must be required after to allow compact name style
require 'neruda/url'
require 'neruda/chapter'

# Main Sinatra application
class Neruda::App < Sinatra::Base
  configure :production, :development do
    # When used as a Gem, we lose the correct path.
    set :root, Dir.pwd
    enable :logging
    disable :method_override, :sessions
    mime_type :epub, 'application/epub+zip'
  end

  include Neruda::Url
  include Neruda::Chapter

  def find_slug
    @slug = params[:chapter]
    halt 404 if @slug.nil?
  end

  def find_file(kind = 'org')
    f = File.join('private', "#{kind}s", "#{@slug}.#{kind}")
    halt 404 unless File.exist? f
    f
  end

  def find_chapter
    @org_file = find_file
  end

  get '/epub/:chapter' do
    find_slug
    if @slug == 'all' && !Neruda::CONFIG['book_filename'].nil?
      @slug = Neruda::CONFIG['book_filename']
    end
    epub_file = find_file('epub')
    halt 404 unless File.exist? epub_file
    content_type :epub
    send_file epub_file, filename: "#{@slug}.epub"
  end

  get '/chapter/:chapter' do
    find_slug
    find_chapter
    @content = Orgmode::Parser.load @org_file
    title # Force the early removal of the title
    slim :chapter
  end

  unless Neruda::CONFIG['base_path'].nil?
    # If we are behind a misconfigured subfolder reverse proxy, this one
    # could be usefull
    get Neruda::CONFIG['base_path'] do
      call env.merge('PATH_INFO' => '/')
    end
  end

  get '/' do
    @slug = 'index'
    text_content = ''
    if File.exist? File.join('private', 'orgs', 'index.org')
      find_chapter
      @content = Orgmode::Parser.load @org_file
      text_content = @content.to_html
    end

    @chapters = []
    if File.exist? File.join('config', 'chapters.yml')
      @chapters = YAML.load_file(File.join('config', 'chapters.yml'))
    end

    slim :index, locals: { text: text_content }
  end

  error 403 do
    slim 'h1 Access forbidden'
  end

  error 404 do
    slim 'h1 Not Found'
  end
end
