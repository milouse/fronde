# frozen_string_literal: true

require 'neruda/config'

module Neruda
  # This module holds HTML formatter methods for the {Neruda::OrgFile}
  # class.
  module OrgFileHtmlizer
    # Output current file content as a HTML formatted string.
    #
    # @return [String] the file content in HTML
    def to_html
      backup = setup_chunk_dir
      IO.write(@file, @content)
      call_emacs ['--eval "(org-html-export-to-html nil nil nil t)"']
      result = IO.read(@html_file).strip
      remove_chunk_dir(backup)
      result
    end

    # Publish the current file or the entire project if
    #   {Neruda::OrgFile#file @file} is ~nil~.
    #
    # @return [Boolean, nil] the underlying ~system~ method return value
    def publish
      if @file.nil?
        emacs_args = ['--eval \'(org-publish "website")\'']
      else
        emacs_args = ['-f org-publish-current-file']
      end
      call_emacs emacs_args
    end

    private

    # Format {Neruda::OrgFile#keywords} list in an HTML listing.
    #
    # @return [String] the HTML keywords list
    def keywords_to_html
      domain = Neruda::Config.settings['domain']
      klist = @keywords.map do |k|
        <<~KEYWORDLINK
          <li class="keyword">
            <a href="#{domain}/tags/#{Neruda::OrgFile.slug(k)}.html">#{k}</a>
          </li>
        KEYWORDLINK
      end.join
      "<ul class=\"keywords-list\">#{klist}</ul>"
    end

    # Format {Neruda::OrgFile#date} as a HTML `time` tag.
    #
    # @return [String] the HTML `time` tag
    def date_to_html(dateformat = :full)
      return '' if @date.nil?
      "<time datetime=\"#{@date.rfc3339}\">#{datestring(dateformat)}</time>"
    end

    # Format {Neruda::OrgFile#author} in a HTML `span` tag with a
    #   specific class.
    #
    # @return [String] the author HTML `span`
    def author_to_html
      return '' if @author == ''
      "<span class=\"author\">#{@author}</span>"
    end

    def emacs_command(arguments = [])
      default_emacs = Neruda::Config.settings['emacs']
      emacs_cmd = [default_emacs || 'emacs -Q -q --batch -nw']
      emacs_cmd << '-l ./org-config.el' if File.exist?('org-config.el')
      unless @options[:verbose]
        emacs_cmd << '--eval \'(setq inhibit-message t)\''
      end
      emacs_cmd << "--eval '(find-file \"#{@file}\")'" unless @file.nil?
      emacs_cmd.concat(arguments)
      emacs_cmd.join(' ')
    end

    def call_emacs(arguments = [])
      command = emacs_command arguments
      if @options[:verbose]
        warn command
        return system(command)
      end
      system command, out: '/dev/null', err: '/dev/null'
    end

    def setup_chunk_dir
      old_file = @file
      old_html_file = @html_file
      tmp_dir = "#{Dir.pwd}/tmp"
      FileUtils.mkdir(tmp_dir) unless Dir.exist?(tmp_dir)
      @file = "#{tmp_dir}/chunk.org"
      @html_file = @file.sub(/\.org$/, '.html')
      [tmp_dir, old_file, old_html_file]
    end

    def remove_chunk_dir(backup)
      FileUtils.rm [@file, @html_file]
      FileUtils.rm_r backup[0] if Dir.empty?(backup[0])
      @file = backup[1]
      @html_file = backup[2]
    end
  end
end
