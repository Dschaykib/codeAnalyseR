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

  # lockfile <- "renv.lock"
  # lockfile <- "../flowchart/renv.lock"

# input checks ------------------------------------------------------------

  if (!file.exists(lockfile)) {
    stop(paste0("lockfile: '", lockfile, "' not found!"))
  }

  if (basename(lockfile) != "renv.lock") {
    stop(paste0("lockfile is not a 'renv.lock' file!"))
  }



# load renv file and dependencies -----------------------------------------

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

  pkg_list <- lapply(renv_pkgs$Packages, extract_pkg_info)

  pkg_dt <- data.table::rbindlist(pkg_list)

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

  links <- unique(data.frame(
    "source" = nodes_dt$name[match(pkg_dt$pkg, nodes_dt$name)],
    "target" = nodes_dt$name[match(pkg_dt$dep, nodes_dt$name)],
    "IDsource" = nodes_dt$id[match(pkg_dt$pkg, nodes_dt$name)] - 1,
    "IDtarget" = nodes_dt$id[match(pkg_dt$dep, nodes_dt$name)] - 1,
    "value" = 1
  ))

  links <- links[!is.na(links$target) & !is.na(links$source), ]


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

  used_colors_nodes <- c(
    "leaf" = "red",
    "node" = "darkgreen",
    "start" = "blue")

  used_colors_source <- rainbow(data.table::uniqueN(nodes_dt$source))
  names(used_colors_source) <- unique(nodes_dt$source)

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
        seq_along(used_colors_source) * 15,
        ').',
        'attr("r", 6).style("fill", "',
        used_colors_source,'")', collapse = "\n"),
      paste0(
        'svg.append("text").attr("x", 10).attr("y", ',
        seq_along(used_colors_source) * 15,
        ').text("',
        names(used_colors_source),'").style("font-size", "10px").',
        'attr("alignment-baseline","middle")', collapse = "\n"),

      '}'),
      collapse = "\n"
    )

  # cat(JS)
  p <- htmlwidgets::onRender(p, JS)

  return(p)

}

