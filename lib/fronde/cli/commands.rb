# frozen_string_literal: true

require_relative 'helpers'
require_relative 'optparse'
require_relative '../index/slug'

module Fronde
  module CLI
    # Fronde commands
    module Commands
      def fronde_update
        Helpers.update_config @options
        @rake.options.build_all = true
        @rake.invoke_task('org:upgrade')
      end
      alias_method :fronde_config, :fronde_update

      def fronde_init
        Helpers.update_config @options
        @rake.options.build_all = true
        @rake.invoke_task('org:install')
        return if File.exist? 'src/index.org'
        Fronde::OrgFile.new('src/index.org', @options).write
        fronde_open 'src/index.org'
      end

      def fronde_build
        @rake.options.build_all = true
        task = 'site:build'
        task = "#{task}[true]" if @options[:force]
        @rake.invoke_task task
      end

      def fronde_preview
        Thread.new do
          sleep 1
          port = Fronde::CONFIG.get(%w[preview server_port], 5000)
          Helpers.launch_app_for_uri "http://127.0.0.1:#{port}/"
        end
        @rake.invoke_task('site:preview')
      end

      def fronde_open(file_path = ARGV[0])
        editor = ENV['EDITOR'] || ENV['VISUAL'] || 'emacs'
        cmd = [editor]
        file_path ||= Dir.pwd
        unless File.file?(file_path)
          # file_path may be updated with title given in options
          file_path = create_new_file(file_path)
          # Only move to the end of file for new file. Let the editor handle
          # the best position for already existing files
          cmd << '+6'
        end
        cmd << file_path
        system(*cmd)
      end
      alias_method :fronde_edit, :fronde_open

      # TODO
      def fronde_publish(publication_format = 'html')
        unless %w[html gemini].include?(publication_format)
          publication_format = 'html'
        end
        @rake.invoke_task('sync:push')
      end

      def fronde_help(command = 'basic')
        cmd_opt = OptParse.command_options(command)
        label = cmd_opt[:label] || command
        warn format("%<label>s\n\n", label: R18n.t.fronde.bin.usage(label))
        cmd = cmd_opt[:name] || command
        if R18n.t.fronde.bin.commands[cmd].translated?
          warn format("%<label>s\n\n", label: R18n.t.fronde.bin.commands[cmd])
        end
        warn OptParse.help_command_body(cmd).join("\n")
      end

      private

      def file_name_from_title
        title = @options[:title] || R18n.t.fronde.bin.options.default_title
        # No title, nor a reliable file_path? Better abort
        if title == ''
          warn R18n.t.fronde.error.bin.no_file
          exit 1
        end
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
        Fronde::OrgFile.new(filename, @options).write
        filename
      end
    end
  end
end
