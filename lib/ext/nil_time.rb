# frozen_string_literal: true

# A time emulator to handle cases where no time is available
class NilTime
  def strftime(*)
    ''
  end

  def xmlschema(_ = 0)
    ''
  end

  %i[l18n_short_date_string l18n_long_date_string
     l18n_long_date_no_year_string].each do |name|
    define_method(name) { '' }
  end

  def l18n_short_date_html
    '<time></time>'
  end
  alias_method :l18n_long_date_html, :l18n_long_date_string
end
