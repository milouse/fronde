# frozen_string_literal: true

# Various method to handle org conversion and metadata access
module Neruda::Chapter
  def meta_data(key)
    return nil if @content.nil?
    value = @content.in_buffer_settings[key.upcase]
    return value if value.nil? || !key.casecmp('date').zero?
    time_for(value).strftime('%A %d %B %Y')
  end

  def title
    return Neruda::CONFIG['title'] if @content.nil?
    return @title unless @title.nil?
    # We use an instance variable to avoid Orgmode::Parser to render
    # title with the rest of the file
    # Thus we are removing title from the buffer_settings
    @title = @content.in_buffer_settings.delete('TITLE')
    return @title unless @title.nil?
    @title = Neruda::CONFIG['title']
  end

  def author
    meta_data('author') || Neruda::CONFIG['author']
  end
end
