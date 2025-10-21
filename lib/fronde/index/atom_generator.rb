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
        puts I18n.t('fronde.index.atom_generated', tag:) if verbose
      end
    end

    private

    # Render an Atom feed file.
    #
    # @param tag_name [String] the tag name of the current atom feed
    # @param entries [Array] the article to list in this file
    # @return [String] the Atom feed as a String
    def atom_file(tag_name, entries)
      slug = Slug.slug(tag_name)
      variables = atom_templating_basics.merge(
        'title' => @tags_names[tag_name],
        'slug' => slug,
        'entries' => entries
      )
      Config::Helpers.render_liquid_template(
        File.read(File.expand_path('./data/template.xml', __dir__)),
        variables
      )
    end

    # Render the main/index Atom feed.
    #
    # @param entries [Array] the article to list in this file
    # @return [String] the Atom feed as a String
    def atom_index(entries)
      variables = atom_templating_basics.merge(
        'title' => @project['title'],
        'slug' => '__HOME_PAGE__',
        'entries' => entries
      )
      Config::Helpers.render_liquid_template(
        File.read(File.expand_path('./data/template.xml', __dir__)),
        variables
      )
    end

    def atom_templating_basics
      {
        'lang' => Fronde::CONFIG.get('lang'),
        'author' => Fronde::CONFIG.get('author'),
        'domain' => Fronde::CONFIG.get('domain'),
        'project_path' => @project.public_absolute_path,
        'upddate' => @date.xmlschema,
        'publication_format' => @project['mime_type']
      }
    end
  end
end
