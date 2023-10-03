# frozen_string_literal: true

require_relative '../config'

module Fronde
  # Embeds Atom feeds sepecific methods
  module IndexAtomGenerator
    def to_atom(index_name = 'index')
      entries = @index[index_name][0...10].map(&:to_h)
      return atom_index(entries) if index_name == 'index'

      atom_file(index_name, entries)
    end

    def write_atom(index_name)
      return unless save?
      slug = Slug.slug index_name
      pubdir = Fronde::CONFIG.get("#{@pub_format}_public_folder")
      FileUtils.mkdir_p "#{pubdir}/feeds"
      atomdest = "#{pubdir}/feeds/#{slug}.xml"
      File.write(atomdest, to_atom(index_name))
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
      tagurl = "#{domain}/tags/#{slug}.html"
      Config::Helpers.render_liquid_template(
        File.read(File.expand_path('./data/template.xml', __dir__)),
        'title' => @tags_names[tag_name],
        'lang' => Fronde::CONFIG.get('lang'),
        'domain' => domain,
        'slug' => slug,
        'tagurl' => tagurl,
        'upddate' => @date.xmlschema,
        'author' => Fronde::CONFIG.get('author', ''),
        'publication_format' => @pub_format,
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
        'title' => Fronde::CONFIG.get('title', R18n.t.fronde.index.all_tags),
        'lang' => Fronde::CONFIG.get('lang'),
        'domain' => domain,
        'slug' => 'index',
        'tagurl' => domain,
        'upddate' => @date.xmlschema,
        'author' => Fronde::CONFIG.get('author', ''),
        'publication_format' => @pub_format,
        'entries' => entries
      )
    end
  end
end
