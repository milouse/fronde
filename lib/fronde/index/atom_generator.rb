# frozen_string_literal: true

require_relative '../config'

module Fronde
  # Reopen Index class to embed Atom feeds sepecific methods
  class Index
    def to_atom(index_name = 'index')
      entries = @index[index_name][0...10].map(&:to_h)
      return atom_index(entries) if index_name == 'index'

      atom_file(index_name, entries)
    end

    def write_atom(index_name)
      slug = Slug.slug index_name
      atomdest = "#{@project.publication_path}/feeds/#{slug}.xml"
      File.write atomdest, to_atom(index_name)
    end

    def write_all_feeds(verbose: true)
      FileUtils.mkdir_p "#{@project.publication_path}/feeds"
      @index.each_key do |tag|
        write_atom(tag)
        warn I18n.t('fronde.index.atom_generated', tag:) if verbose
      end
    end

    private

    # Render an Atom feed file.
    #
    # @param tag_name [String] the tag name of the current atom feed
    # @param entries [Array] the article to list in this file
    # @return [String] the Atom feed as a String
    def atom_file(tag_name, entries)
      domain = Fronde::CONFIG.get('domain')
      slug = Slug.slug(tag_name)
      tagurl = "#{domain}#{@project.public_absolute_path}tags/#{slug}.html"
      Config::Helpers.render_liquid_template(
        File.read(File.expand_path('./data/template.xml', __dir__)),
        'title' => @tags_names[tag_name],
        'lang' => Fronde::CONFIG.get('lang'),
        'domain' => domain,
        'slug' => slug,
        'tagurl' => tagurl,
        'upddate' => @date.xmlschema,
        'author' => Fronde::CONFIG.get('author'),
        'publication_format' => @project['mime_type'],
        'entries' => entries
      )
    end

    # Render the main/index Atom feed.
    #
    # @param entries [Array] the article to list in this file
    # @return [String] the Atom feed as a String
    def atom_index(entries)
      domain = Fronde::CONFIG.get('domain')
      Config::Helpers.render_liquid_template(
        File.read(File.expand_path('./data/template.xml', __dir__)),
        'title' => @project['title'],
        'lang' => Fronde::CONFIG.get('lang'),
        'domain' => domain,
        'slug' => 'index',
        'tagurl' => domain,
        'upddate' => @date.xmlschema,
        'author' => Fronde::CONFIG.get('author'),
        'publication_format' => @project['mime_type'],
        'entries' => entries
      )
    end
  end
end
