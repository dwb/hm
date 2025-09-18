local wezterm = require 'wezterm'

-- wezterm.gui is not available to the mux server, so take care to
-- do something reasonable when this config is evaluated by the mux
function get_appearance()
  if wezterm.gui then
    return wezterm.gui.get_appearance()
  end
  return 'Dark'
end

function scheme_for_appearance(appearance)
  if appearance:find 'Dark' then
    return 'zenwritten_dark'
  else
    return 'zenwritten_light'
  end
end


return {
  front_end = "WebGpu", -- needed for VMs
  font = wezterm.font("Iosevka DWB Term"),
  font_size = 12.0,
  color_scheme = scheme_for_appearance(get_appearance()),
  initial_cols = 100,
  initial_rows = 55,
}
