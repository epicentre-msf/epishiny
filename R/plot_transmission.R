plot_transmission_chains <- function(
  epi_obj,
  x_axis = NULL,
  node_size = 15,
  node_color = NULL,
  edge_width = 2.5,
  edge_color = NULL,
  arrow_size = 0.5,
  timeline_view = TRUE,
  tl_start = NULL,
  tl_end = NULL,
  tl_edge_width = 3,
  tl_end_node_size = 12,
  tl_edge_color = NULL,
  tl_edge_label = NULL,
  rank_contact = NULL,
  network_shape = "rectangle",
  label = FALSE,
  font_size = 15,
  n_date_break = 20,
  date_labels = c("%d\n%B"),
  height = 1500,
  width = 1200,
  selector = "id"
) {
  # Create timeline object if timeline view is enabled
  if (timeline_view) {
    timeline <- epi_obj$linelist |>
      dplyr::transmute(
        id,
        start = .data[[tl_start]],
        end = .data[[tl_end]]
      )
  } else {
    timeline <- NULL
  }

  # Create plot
  tree <- plot(
    epi_obj,
    x_axis = x_axis,

    # node
    node_size = node_size,
    node_color = node_color,

    # edges
    edge_color = edge_color,
    edge_width = edge_width,

    network_shape = network_shape,
    parent_pos = "top",

    # timeline
    timeline = timeline,
    tl_end_node_size = tl_end_node_size,
    tl_edge_width = tl_edge_width,
    tl_edge_color = tl_edge_color,
    tl_edge_label = tl_edge_label,

    arrow_size = arrow_size,
    font_size = font_size,
    date_labels = date_labels,
    height = height,
    width = width,
    n_break = n_date_break,
    position_dodge = TRUE,
    selector = selector
  )

  return(tree)
}

# TEST ZONE
#
# #Example usage:
# epi_sim <- episimdata::evd_data
#
# # Basic usage with timeline view
# plot_transmission_chains(
#   epi_sim,
#   rank_contact = "t_onset",
#   x_axis = "t_onset",
#   label = "id",
#   timeline_view = TRUE,
#   tl_start = "t_onset",
#   tl_end = "t_outcome",
#   node_color = "sex",
#   network_shape = "rectangle",
#   n_date_break = 20
# )
