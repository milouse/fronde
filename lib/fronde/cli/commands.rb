# frozen_string_literal: true

require 'shellwords'
require_relative 'helpers'
require_relative 'opt_parse'
require_relative '../slug'
require_relative '../org/file'

module Fronde
  module CLI
    # Fronde commands
    module Commands
      def fronde_new
        new_dir = @argv.first || 'my_fronde_website'
        FileUtils.mkdir new_dir
        Dir.chdir new_dir
        Helpers.init_config_file @options
        Fronde::CONFIG.reset # Correctly compute various path
        Helpers.init_rakefile
        init_rake
        @rake['org:install'].invoke
        @argv = ['src/index.org']
        fronde_open
      end

      def fronde_update
        Helpers.init_rakefile
        init_rake
        @rake.options.build_all = true
        @rake['org:upgrade'].invoke
        true
      end

      def fronde_build
        @rake.options.build_all = true
        @rake['site:build'].invoke @options[:force]
        true
      end

      def fronde_preview
        Thread.new do
          sleep 1
          port = Fronde::CONFIG.get(%w[preview server_port], 5000)
          Helpers.launch_app_for_uri "http://127.0.0.1:#{port}/"
        end
        @rake['site:preview'].invoke
        true
      end

      def fronde_open
        cmd = (ENV['EDITOR'] || ENV['VISUAL'] || 'emacs').shellsplit
        file_path = @argv.first || Dir.pwd
        unless File.file?(file_path)
          # file_path may be updated with title given in options
          file_path = create_new_file(file_path)
          # Only move to the end of file for new file. Let the editor handle
          # the best position for already existing files
          cmd << '+6'
        end
        cmd << file_path
        system(*cmd, exception: true)
      end

      def fronde_publish
        @rake['sync:push'].invoke
        true
      end

      def fronde_help
        # Try to find command in next argv, otherwise fallback again.
        @command = @argv.shift || 'basic' if @command == 'help'
        cmd_opt = OptParse.command_options(@command)
        label = cmd_opt[:label] || @command
        output = [format_label(I18n.t('fronde.bin.usage', label:))]
        cmd = cmd_opt[:name] || @command
        output << format_label(I18n.t("fronde.bin.commands.#{cmd}"))
        output << OptParse.help_command_body(cmd)
        puts output.join
        true
      end

      private

      def format_label(label)
        return '' if label == '' || label.start_with?('Translation missing:')

        format("%<label>s\n\n", label:)
      end

      def file_name_from_title
        title = @options[:title] || I18n.t('fronde.bin.options.default_title')
        # No title, nor a reliable file_path? Better abort
        raise I18n.t('fronde.error.bin.no_file') if title == ''

        "#{Fronde::Slug.slug(title)}.org"
      end

      def new_file_name(file_path)
        file_path = File.expand_path(file_path)

        if file_path[-4..] == '.org' && !File.directory?(file_path)
          return file_path
        end

        # file_path seems to be a dir path. Thus we have to create the new
        # filename from its title
        File.join file_path, file_name_from_title
      end

      def create_new_file(file_path)
        filename = new_file_name(file_path)
        FileUtils.mkdir_p File.dirname(filename)
        Fronde::Org::File.new(filename, @options).write
        filename
      end
    end
  end
end
