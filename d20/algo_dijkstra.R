# Fonction principale pour Dijkstra
dijkstra <- function(grid, start, end) {
  # Initialisation
  n_rows <- nrow(grid)
  n_cols <- ncol(grid)
  distances <- matrix(Inf, nrow = n_rows, ncol = n_cols)
  visited <- matrix(FALSE, nrow = n_rows, ncol = n_cols)
  distances[start[1], start[2]] <- 0  # La distance de départ est 0

  directions <- list(
    c(-1, 0), c(1, 0),  # Haut, Bas
    c(0, -1), c(0, 1)   # Gauche, Droite
  )

  # Tant qu'il reste des noeuds non visités
  while (TRUE) {
    # Trouver le noeud non visité avec la plus petite distance
    current <- which(distances == min(distances[!visited]), arr.ind = TRUE)
    if (length(current) == 0 || all(visited[end[1], end[2]])) {
      break
    }

    current <- current[1, , drop = FALSE]  # Extraire les coordonnées
    cx <- current[1]
    cy <- current[2]
    visited[cx, cy] <- TRUE

    # Mettre à jour les distances des voisins
    for (dir in directions) {
      nx <- cx + dir[1]
      ny <- cy + dir[2]

      if (nx > 0 && ny > 0 && nx <= n_rows && ny <= n_cols &&
          !visited[nx, ny] && grid[nx, ny] != "1") {
        new_distance <- distances[cx, cy] + 1
        if (new_distance < distances[nx, ny]) {
          distances[nx, ny] <- new_distance
        }
      }
    }
  }

  return(distances = distances)
}
