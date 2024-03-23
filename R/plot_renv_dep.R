plot_renv_dep <- function(lockfile = "renv.lock") {
  # libraries
  library(ggraph)
  library(igraph)

  lockfile <- "../caRdoon/renv.lock"
  if (!file.exists(lockfile)) {
    stop(paste0("lockfile: '", lockfile, "' not found!"))
  }

  renv_pkgs <- renv::lockfile_read(file = lockfile)

  pkg_list <- lapply(renv_pkgs$Packages, function(pkg) {
    #pkg <- renv_pkgs$Packages[[1]]
    deps <- pkg$Requirements
    this_source <- ifelse(pkg$Source == "Repository",
                          pkg$Repository,
                          pkg$Source
    )

    if (length(deps) == 0) {
      out <- data.frame()
    } else {
      out <- data.frame(
        pkg = pkg$Package,
        dep = deps,
        source = this_source,
        stringsAsFactors = FALSE
      )

    }
    return(out)
  })

  pkg_dt <- data.table::rbindlist(pkg_list)
  pkg_dt[, num_deps := .N, by = pkg]
  pkg_dt[, top_pkg := !(pkg %in% dep)]
  # keep all deps and pkgs
  #pkg_dt <- pkg_dt[!(dep == "R" & top_pkg == FALSE),]

  nodes_dt <- data.table::data.table(
    id = seq_len(data.table::uniqueN(c(pkg_dt$pkg, pkg_dt$dep))),
    name = unique(c(pkg_dt$pkg, pkg_dt$dep)),
    type = "node",
    source = "-"
  )

  all_packages <- nodes_dt$name

  #r_node <- nodes_dt$id[nodes_dt$name == "R"]
  nodes_dt <- nodes_dt[nodes_dt$name != "R", ]

  # adjust types
  nodes_dt[nodes_dt$name %in% pkg_dt$pkg &
             !nodes_dt$name %in% pkg_dt$dep, type := "start"]
  nodes_dt[!nodes_dt$name %in% pkg_dt$pkg &
             nodes_dt$name %in% pkg_dt$dep, type := "leaf"]

  # adjust source
  pkg_dt_tmp <- unique(pkg_dt[, list(pkg, source)])
  pkg_idx <- match(nodes_dt$name, pkg_dt_tmp$pkg)
  nodes_dt$source[!is.na(pkg_idx)] <-
    pkg_dt_tmp$source[c(na.omit(pkg_idx))]


  edges <- unique(data.frame(
    "to" = nodes_dt$id[match(pkg_dt$dep, nodes_dt$name)],
    "from" = nodes_dt$id[match(pkg_dt$pkg, nodes_dt$name)]
  ))

  # edges <- edges[edges$to != r_node & edges$from != r_node, ]
  edges <- edges[!is.na(edges$to) & !is.na(edges$from), ]


  edge_types <- data.table::fifelse(
    test = edges$to %in% nodes_dt[type == "leaf", id],
    yes = "leaf",
    no = "node")
  edge_types <- data.table::fifelse(
    test = edges$to %in% nodes_dt[type == "start", id],
    yes = "start",
    no = edge_types)

  # adjsut names to contain number of deps
  nodes_dt$name <- unlist(
    lapply(
      X = nodes_dt$name,
      FUN =  function(x) {
        paste0(x, " (",
               sum(pkg_dt$pkg == x),
               "|",
               sum(pkg_dt$dep == x),
               ")")
      }
    )
  )


  # Create a graph object
  mygraph <- graph_from_data_frame(
    d = edges,
    directed = TRUE,
    vertices = nodes_dt
  ) %>%
    set_edge_attr(
      name = "edge_color",
      value = edge_types) %>%
    set_vertex_attr(
      name = "node_source",
      value = nodes_dt$source)

  used_colors_nodes <- c(
    "leaf" = "red",
    "node" = "darkgreen",
    "start" = "blue")

  used_colors_source <- rainbow(data.table::uniqueN(nodes_dt$source))
  names(used_colors_source) <- unique(nodes_dt$source)


  # # Basic tree
  # ggraph(mygraph, layout = 'dendrogram', circular = FALSE) +
  #   geom_edge_diagonal() +
  #   geom_node_point()

  ggraph::ggraph(mygraph, layout = 'auto', circular = FALSE) +
    ggraph::geom_edge_link(aes(color = edge_color)) +
    # ggraph::geom_node_point(aes(color = node_source), size = 4) +
    ggraph::geom_node_label(aes(label = name,
                                fill = source),
                            color = "white",
                            repel = TRUE,
                            max.overlaps = Inf) +
    ggraph::scale_edge_colour_manual(values = used_colors_nodes) +
    # ggplot2::scale_color_manual(values = used_colors_nodes) +
    ggplot2::scale_color_manual(values = used_colors_source) +
    # ggplot2::scale_fill_manual(values = used_colors_nodes) +
    ggplot2::scale_fill_manual(values = used_colors_source) +
    ggtitle("Packagename (dependencies | imports)")




}
