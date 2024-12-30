export-env {
  $env.config.menus = ($env.config.menus | where name != my_glob_menu | append {
    name: my_glob_menu
    only_buffer_difference: true
    marker: "* "
    type: {
        layout: list
        page_size: 10
    }
    style: {
        text: green
        selected_text: green_reverse
        description_text: yellow
    }
    source: { |buffer, position|
        history -l
        | reverse
        | where session_id == (history session)
        | each { |i|
          ast --flatten $i.command
          | where shape == shape_globpattern
          | each { {value: $in.content description: $i.command} }
        }
        | flatten
    }
  })

  $env.config.keybindings = ($env.config.keybindings | where name != my_glob_menu | append {
    name: my_glob_menu
    modifier: control

    keycode: char_k
    mode: [emacs vi_insert vi_normal]
    event: {
      until: [
        { send: Menu name: my_glob_menu }
        { send: MenuPageNext }
      ]
    }
  })
}
