# frozen_string_literal: true

module R18nPatch
  # Monkey patch to add a little helper
  refine R18n::Translation do
    def full_datetime_format
      date_fmt = fronde.index.full_date_format(
        date: @locale.full_format
      )
      date_fmt = @locale.year_format.sub('_', date_fmt)
      time_fmt = @locale.time_format.delete('_').strip
      fronde.index.full_date_with_time_format(
        date: date_fmt, time: time_fmt
      )
    end
  end
end
