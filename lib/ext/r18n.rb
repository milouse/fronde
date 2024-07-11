# frozen_string_literal: true

# Monkey patch to add a little helper
module R18nPatch
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

    # Returns the given Time instance as a localized long string.
    #
    # @param time [Time] the Time instance to format
    # @param with_year [Boolean] whether or not the string must contain
    #   the year
    # @param with_time [Boolean] whether or not the string must contain
    #   the time
    # @return [String] the localized Time string representation
    def long_date_string(time, with_year: true, with_time: true)
      long_fmt = fronde.index.full_date_format(
        date: @locale.format_date_full(time, year: with_year)
      )
      if with_time
        long_fmt = fronde.index.full_date_with_time_format(
          date: long_fmt, time: @locale.time_format.delete('_').strip
        )
      end
      @locale.strftime(time, long_fmt)
    end
  end
end
