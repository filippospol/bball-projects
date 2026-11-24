# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# {gt} table + {ggplot2} theme reference (light & dark), with HnR branding.
# Author: Filippos Polyzos
#
# Fonts are pulled ONLINE -- nothing has to be installed locally:
#   - Roboto Mono  via Google Fonts (font_add_google)
#   - Font Awesome (Brands) fetched from the Font Awesome repo to a tempfile, then registered
#     with sysfonts, so the caption icon works on any machine with no local font download.
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

pacman::p_load(tidyverse,gt,gtExtras,sysfonts,showtext,ggtext)

mydf = mtcars   # example data

# ---- Branding: online fonts (no local install) ---------------------------------------------
# Roboto Mono for both gt tables and ggplot charts.
font_add_google("Roboto Mono","myfont")

# Font Awesome Brands straight from the repo -> tempfile -> register as "FA". sysfonts::font_add
# needs a font FILE (not a URL), so we download once per session. Swap the pinned tag to bump
# versions; swap the &#x glyph for any brand icon (https://fontawesome.com/icons?f=brands).
fa_url  = "https://raw.githubusercontent.com/FortAwesome/Font-Awesome/6.7.2/webfonts/fa-brands-400.ttf"
fa_file = file.path(tempdir(),"fa-brands-400.ttf")
if (!file.exists(fa_file)) download.file(fa_url,fa_file,mode="wb",quiet=TRUE)
sysfonts::font_add("FA",fa_file)
showtext_auto()

# Captions. gt renders to HTML so it loads Font Awesome from a CDN; ggplot renders the glyph
# through the "FA" font registered above. Both show the same bird mark (&#xf099; / fa-twitter).
mycaption = '<meta name="viewport" content="width=device-width, initial-scale=1"> <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.7.2/css/all.min.css"> <i class="fa-brands fa-twitter"></i> filippos_pol'
pc        = "<span style='font-family:FA;'>&#xf099;</span> @filippos_pol"

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# {gt} table
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# ---- Regular -------------------------------------------------------------------------------
head(mydf) %>%
  gt() %>%
  cols_align(align = "center") %>%                                     # columns alignment
  cols_label() %>%                                                     # column labels (old = new)
  tab_header(                                                          # title + logo (top-right)
    title = htmltools::HTML(
      paste0(
        "<div style='display:flex; justify-content:space-between; align-items:center;'>",
        "<div style='font-size:25px'><b>head(10) mtcars table</b><br>
         <span style='font-weight:normal;color:#8e8e8e;font-size:16px'>Example table using my theme</span></div>",
        "<div><img src='https://i.ibb.co/TxnGsfZm/Hn-R-No-Backround-Black.png' style='height:80px;'></div>",
        "</div>"
      )
    )
  ) %>%
  tab_style(style = cell_text(weight = "bold"),                        # bold column labels
            locations = cells_column_labels(everything())) %>%
  gt_highlight_rows(rows = 2, font_weight = "normal") %>%              # highlight a row
  gt_color_rows(c(qsec), palette = c("#1f77b4", "#aec7e8")) %>%        # colour a column's rows
  tab_source_note(source_note = md(paste("Table by: Filippos Polyzos |",mycaption))) %>%
  tab_options(data_row.padding = px(1),                               # design options
              row.striping.background_color = "#eeeeee",
              heading.align = "left",
              heading.title.font.size = 25,
              heading.subtitle.font.size = 18,
              table.font.size = 14,
              table.border.top.color = "transparent",
              heading.border.bottom.color = "transparent",
              column_labels.border.bottom.width = px(3),
              column_labels.border.bottom.color = "#4c4c4c",
              table_body.border.bottom.width = px(3),
              table_body.border.bottom.color = "#4c4c4c",
              table.border.bottom.color = "transparent") %>%
  opt_row_striping() %>%
  opt_table_font(font = google_font("Roboto Mono"))

# ---- Dark ----------------------------------------------------------------------------------
head(mydf) %>%
  gt() %>%
  cols_align(align = "center") %>%
  cols_label() %>%
  tab_header(
    title = htmltools::HTML(
      paste0(
        "<div style='display:flex; justify-content:space-between; align-items:center;'>",
        "<div style='font-size:25px'><b>head(10) mtcars table</b><br>
         <span style='font-weight:normal;color:#cccccc;font-size:16px'>Example table using my theme</span></div>",
        "<div><img src='https://i.ibb.co/QFZd6ftN/Hn-R-No-Backround-White.png' style='height:80px;'></div>",
        "</div>"
      )
    )
  ) %>%
  tab_style(style = cell_text(weight = "bold", color = "white"),
            locations = cells_column_labels(everything())) %>%
  gt_highlight_rows(rows = 2, fill = "yellow4",                        # stands out on dark
                    font_weight = "normal", font_color = "white") %>%
  gt_color_rows(c(qsec), palette = c("#1f77b4", "#aec7e8")) %>%
  tab_source_note(source_note = md(paste("Table by: Filippos Polyzos |", mycaption))) %>%
  tab_options(data_row.padding = px(1),
              row.striping.background_color = "#2a2a2a",
              heading.align = "left",
              heading.title.font.size = 25,
              heading.subtitle.font.size = 18,
              table.font.size = 14,
              table.border.top.color = "transparent",
              heading.border.bottom.color = "transparent",
              column_labels.border.bottom.width = px(3),
              column_labels.border.bottom.color = "#555555",
              table_body.border.bottom.width = px(3),
              table_body.border.bottom.color = "#555555",
              table.border.bottom.color = "transparent",
              table.background.color = "#1e1e1e",
              heading.background.color = "#1e1e1e",
              column_labels.background.color = "#1e1e1e") %>%
  opt_row_striping() %>%
  tab_style(style = cell_text(color = "white"), locations = cells_body()) %>%
  tab_style(style = cell_text(color = "#aaaaaa"), locations = cells_source_notes()) %>%
  opt_table_font(font = google_font("Roboto Mono"))

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# {ggplot2}
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

baseplot = mydf %>%
  ggplot(aes(y=qsec,x=as.factor(gear),fill=gear)) +
  geom_violin() +
  labs(title=md("**mt cars Violin plot**"),
       subtitle="Example chart using my theme\n",
       x="Gears",y="1/4 mile time",
       caption=paste0("Chart by: Filippos Polyzos | ",pc))

# ---- Regular -------------------------------------------------------------------------------
baseplot +
  theme_minimal(base_family="myfont") +
  theme(legend.position="none",
        text=element_text(family="myfont"),
        plot.title=element_markdown(face="bold"),
        plot.subtitle=element_text(color="#8e8e8e"),
        plot.caption=element_markdown(color="#777777",hjust=0))

# ---- Dark ----------------------------------------------------------------------------------
baseplot +
  theme_minimal(base_family="myfont") +
  theme(
    panel.background = element_rect(fill = "#222222", color = NA),
    plot.background = element_rect(fill = "#222222", color = NA),
    legend.position = "none",
    text = element_text(color = "white"),
    plot.title = element_markdown(face = "bold", color = "white"),
    plot.subtitle = element_text(color = "#cccccc"),
    plot.caption = element_markdown(color = "#aaaaaa", hjust = 0),
    axis.text = element_text(color = "white"),
    axis.title = element_text(color = "white"),
    panel.grid.major = element_line(color = "#444444"),
    panel.grid.minor = element_line(color = "#333333")
  )
