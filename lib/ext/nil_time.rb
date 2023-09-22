# frozen_string_literal: true

# A time emulator to handle cases where no time is available
class NilTime
  def strftime(_)
    ''
  end

  def xmlschema(_ = 0)
    ''
  end

  def l18n_short_date_string
    ''
  end

  def l18n_long_date_string(with_year: nil)
    ''
  end

  def l18n_short_date_html
    '<time></time>'
  end
  alias_method :l18n_long_date_html, :l18n_long_date_string
end
