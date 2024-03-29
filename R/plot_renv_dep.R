#' Plotting the dependencies of packages in a renv.lock file
#'
#' @param lockfile a file path to a renv.lock file
#'
#' @return a plot
#'
#' @importFrom renv lockfile_read
#' @import data.table
#' @importFrom networkD3 sankeyNetwork
#' @importFrom htmlwidgets onRender
#'
#' @export
#'
plot_renv_dep <- function(
    lockfile = "renv.lock",
    dep_level = 2) {
  # libraries

  # lockfile <- "renv.lock"
  # lockfile <- "../flowchart/renv.lock"

  if (!file.exists(lockfile)) {
    stop(paste0("lockfile: '", lockfile, "' not found!"))
  }

  if (basename(lockfile) != "renv.lock") {
    stop(paste0("lockfile is not a 'renv.lock' file!"))
  }


  renv_pkgs <- renv::lockfile_read(file = lockfile)
  # get direct dependencies in folder with renv.lock file
  renv_deps <- renv::dependencies(
    path = dirname(lockfile),
    quiet = TRUE,
    dev = TRUE)

  missing_pkgs <- setdiff(renv_deps$Package, names(renv_pkgs$Packages))
  if (length(missing_pkgs) > 0) {
    warning("These packages are a dependency, but are not within renv.lock: ",
            "'", paste0(missing_pkgs, collapse = "', '"), "'")
  }

  extract_pkg_info <- function(pkg) {
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
  }

  pkg_list <- lapply(renv_pkgs$Packages, extract_pkg_info)

  pkg_dt <- data.table::rbindlist(pkg_list)

  #pkg_dt[, num_deps := .N, by = pkg]

  # pkg_dt[, top_pkg := !(pkg %in% dep)]
  # keep all deps and pkgs
  #pkg_dt <- pkg_dt[!(dep == "R" & top_pkg == FALSE),]

  nodes_dt <- data.table::data.table(
    id = seq_len(data.table::uniqueN(c(pkg_dt$pkg, pkg_dt$dep))),
    name = unique(c(pkg_dt$pkg, pkg_dt$dep)),
    type = "node",
    source = "-"
  )

  # remove R from list
  nodes_dt <- nodes_dt[nodes_dt$name != "R", ]

  # set levels of dependencies
  nodes_dt$level <- NA_integer_
  check_pkgs <- unique(renv_deps$Package)
  i_level <- 1L

  for (i_level in seq_len(nrow(nodes_dt))) {
    # break loop if there are no packages left
    if (length(check_pkgs) == 0) break
    nodes_dt[name %in% check_pkgs, level := pmin(level, i_level, na.rm = TRUE)]
    check_pkgs_dep <- pkg_dt[pkg %in% check_pkgs, unique(dep)]
    check_pkgs <- setdiff(check_pkgs_dep, nodes_dt[!is.na(level), name])
  }

  cat("deepest dependency level is", max(nodes_dt$level), "\n")

  # remove level deeper than dep_level to reduce plotted packages
  nodes_dt <- nodes_dt[level <= dep_level, ]
  pkg_dt <- pkg_dt[pkg %in% nodes_dt$name, ]


  all_packages <- nodes_dt$name

  #r_node <- nodes_dt$id[nodes_dt$name == "R"]

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

  links <- unique(data.frame(
    "source" = nodes_dt$name[match(pkg_dt$pkg, nodes_dt$name)],
    "target" = nodes_dt$name[match(pkg_dt$dep, nodes_dt$name)],
    "IDsource" = nodes_dt$id[match(pkg_dt$pkg, nodes_dt$name)] - 1,
    "IDtarget" = nodes_dt$id[match(pkg_dt$dep, nodes_dt$name)] - 1,
    "value" = 1
  ))


  # edges <- edges[edges$to != r_node & edges$from != r_node, ]
  edges <- edges[!is.na(edges$to) & !is.na(edges$from), ]
  links <- links[!is.na(links$target) & !is.na(links$source), ]


  edge_types <- data.table::fifelse(
    test = edges$to %in% nodes_dt[type == "leaf", id],
    yes = "leaf",
    no = "node")
  edge_types <- data.table::fifelse(
    test = edges$to %in% nodes_dt[type == "start", id],
    yes = "start",
    no = edge_types)

  # adjsut names to contain number of deps
  nodes_dt$pkg <- nodes_dt$name
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
  # mygraph <- igraph::graph_from_data_frame(
  #   d = edges,
  #   directed = TRUE,
  #   vertices = nodes_dt
  # ) |>
  #   igraph::set_edge_attr(
  #     name = "edge_color",
  #     value = edge_types) |>
  #   igraph::set_vertex_attr(
  #     name = "node_source",
  #     value = nodes_dt$source)

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

  # out <- ggraph::ggraph(mygraph, layout = 'auto', circular = FALSE) +
  #   ggraph::geom_edge_link(
  #     # ggplot2::aes(color = edge_color)) +
  #     ggplot2::aes(color = "black")) +
  #   # ggraph::geom_node_point(aes(color = node_source), size = 4) +
  #   ggraph::geom_node_label(
  #     ggplot2::aes(label = name, fill = source),
  #     color = "white",
  #     repel = TRUE,
  #     max.overlaps = Inf) +
  #   # ggraph::scale_edge_colour_manual(values = used_colors_nodes) +
  #   # ggplot2::scale_color_manual(values = used_colors_nodes) +
  #   # ggplot2::scale_color_manual(values = used_colors_source) +
  #   # ggplot2::scale_fill_manual(values = used_colors_nodes) +
  #   ggplot2::scale_fill_manual(values = used_colors_source) +
  #   ggplot2::ggtitle(paste0("Packagename (dependencies | imports) for: ", lockfile))



  # sankey plot -------------------------------------------------------------

  links$IDsource <- match(links$source, nodes_dt$pkg)-1
  links$IDtarget <- match(links$target, nodes_dt$pkg)-1

  my_color <- paste0(
    'd3.scaleOrdinal()',
    '.domain([',
    '"', paste0(nodes_dt$source, collapse = '", "'), '"',
    '])',
    '.range([',
    '"', paste0(used_colors_source[nodes_dt$source], collapse = '", "'), '"',
    '])'
  )


  # Make the Network
  p <- networkD3::sankeyNetwork(
    Links = links,
    Nodes = nodes_dt,
    Source = "IDsource",
    Target = "IDtarget",
    Value = "value",
    NodeID = "name",
    colourScale = my_color,
    sinksRight = FALSE)

  JS <-
    paste0(
      c('function(el, x, data){',
      'var svg = d3.select("svg")',
      paste0(
        'svg.append("circle").attr("cx",0).attr("cy", ',
        seq_along(used_colors_source)*15,
        ').',
        'attr("r", 6).style("fill", "',
        used_colors_source,'")', collapse = "\n"),
      paste0(
        'svg.append("text").attr("x", 10).attr("y", ',
        seq_along(used_colors_source)*15,
        ').text("',
        names(used_colors_source),'").style("font-size", "10px").',
        'attr("alignment-baseline","middle")', collapse = "\n"),

      '}'),
      collapse = "\n"
    )

  # cat(JS)
  p <- htmlwidgets::onRender(p,JS)

  return(p)

}
