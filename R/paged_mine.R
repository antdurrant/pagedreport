#' Function for my template
#'
#' @param logo Logo
#' @param front_img Front image
#' @param img_to_dark Darken image
#' @param logo_to_white Tranform logo color to white - logo must be SVG
#' @param other_css Add an other CSS
#' @param ... Arguments passed to pagedown::html_paged
#'
#' @return A pagedown template
#' @export
#'
paged_mine <-
  function(logo = "0",
           front_img = "0",
           img_to_dark = TRUE,
           logo_to_white = FALSE,
           other_css = NULL,
           ...) {
    # arguments
    main_css <-
      pkg_resource("css/style_mine.css")
    pandoc_html <-
      pkg_resource("html/template_paged.html")

    # default img
    if (front_img == "0") {
      front_img <-
        pkg_resource("img/jiyu.jpg")
    }

    if (logo == "0") {
      logo <-
        pkg_resource("logo/logo_kids.svg")
    }

    # darken img
    if (img_to_dark == TRUE) {
      # opacity
      front_img_init <-
        magick::image_read(front_img)
      front_img_ok <-
        magick::image_colorize(front_img_init, opacity = 50, color = "black")

      # path to image
      front_img <- paste0(tempfile("img"), ".jpg")
      magick::image_write(front_img_ok, front_img, format = "jpg")
    }

    # logo to white - logo should be svg
    if (logo_to_white == TRUE) {
      logo_init <- magick::image_read_svg(logo)
      logo_ok <-
        magick::image_colorize(logo_init, opacity = 100, color = "white")

      # path to logo
      logo <- paste0(tempfile("logo"), ".svg")
      magick::image_write(logo_ok, logo, format = "svg")
    }

    # template
    pagedown::html_paged(
      css = c(other_css, main_css),
      template = pandoc_html,
      front_cover = c(logo, front_img),
      back_cover = logo,
      ...
    )
  }
